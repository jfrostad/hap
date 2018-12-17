#!/bin/bash
#
# -----------------------------------------------------------------------------
# Author      : Ian M. Davis
# Group       : LBD
# Date        : 22 Aug 2018
# Summary     :
# This script will determine the appropriate number of threads to use for R
# which has been compiled against Intel's Math Kernel Library (MKL) based on
# the number of slots requested for the job and how many total cores are
# available on the host machine the job lands on through UGE via qsub. After
# this determination, a container is launched from the latest Singularity
# image found in the "image_dir" (unless a specific image is given), the version
# of R in the image is launched, and the specified R script is executed.
#
# It is assumed that this script is launched by an R script (such as
# 'parallelize' or 'make_qsub_share' found in
# 'lbd_core/mbg_central/misc_functions.R') as a qsub job.
#
# This following information is used in this script:
#   * host            : lbd (lbd-cluster-p*) node or prod node (cluster2,
#                       intel, or AMD) as determined from $host value.
#   * NSLOTS          : number of slots requested with the job. UGE sets this
#                       environmental variable with slots requested at the
#                       time of the qsub.
#   * sing_image      : The Singularity image to launch a container from
#                       containing the R MKL version. Default is to located
#                       the latest image found in "image_dir" with the
#                       `get_latest_image()` function
#   * SET_OMP_THREADS : Number of threads allowed for packages with OpenMP
#                       parallel regions (user defined [default=1])
#   * SET_MKL_THREADS : Number of threads allowed for packages linked against
#                       Intel's MKL (user defined
#                       [default=${max_threads} calculated by script])
#   * JOB_NAME        : Job name passed to UGE with qsub
#   * JOB_ID          : Job ID passed to UGE with qsub
#   * /proc/cpuinfo   : Used to find the total number of processors on the host
#                       machine in order to calculate cores/slot
#   * qstat           : Used to grep out total number of slots the machine has
#                       been partitioned into in order to calculate cores/slot
#   * singularity     : This command must be present in order to spin up a
#                       container
#
# Details:
# This script first determines what node it is has landed on (host machine)
# after a `qsub` to execute the R script. It then determines the number of total
# cores total slots on the machine so it can calculate the cores/slot ratio. It
# then determines how many slots have been allocated to the job ($NSLOTS) and,
# using the cores/slot ratio, calculates how many cores it actually has to use
# in the calculation ${max_threads} without oversubscribing the node. Most
# machines have a cores/slot ratio = 1, but there are some machines which do
# not, making the above necessary. Further some types of machines (the AMD's)
# can have a variable number of slots they have been partitioned into (51, 64,
# or 100), giving a variable cores/slot ratio. Since ${max_threads} may not be
# an integer, this script takes some care not to aggressively round up the
# number of cores based on the number slots requested and potentially
# oversubscribe the machine.
#
# After ${max_threads} is determined, the user settings for SET_OMP_THREADS and
# SET_MKL_THREADS (if any) are considered to determine how to set
# OMP_NUM_THREADS and MKL_NUM_THREADS following suggestions from Intel here:
# https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
# R with the MKL works best when both OMP_NUM_THREADS as well as MKL_NUM_THREADS
# are set in such as way as to not oversubscribe the machine. In the event that
# both OMP_NUM_THREADS and MKL_NUM_THREADS are both unset, the MKL will grab all
# available cores on the machine. This is probably undesired behavior on a
# shared cluster node. Since most R functions are not SMP parallel the default
# for this script is to set OMP_NUM_THREADS=1 and
# MKL_NUM_THREADS=${max_threads}. There are some packages (like TMB and INLA)
# which itself has OpenMP parallel regions and each parallel region may also
# make use of the MKL. In such a case, it may make sense for the user to define
# how many threads they wish to set for TMB (with OMP_NUM_THREADS, such as the
# number of physical slots on the machine for example), and a separate number of
# MKL threads (with MKL_NUM_THREADS) which each OpenMP parallel region will
# launch.
#
# Below shows the different configurations that may be set by the user:
#
# Scenario  | User Setting           | Result Set by This Script
# ----------|-----------------------------------------------------------
# A         | SET_OMP_THREADS unset  | OMP_NUM_THREADS=1
# [default] | SET_MKL_THREADS unset  | MKL_NUM_THREADS={max_threads}
# ----------------------------------------------------------------------
# B         | SET_OMP_THREADS=N      | OMP_NUM_THREADS=N (N <= {max_threads})
#           | SET_MKL_THREADS unset  | MKL_NUM_THREADS={max_threads}/N
# ----------------------------------------------------------------------
# C         | SET_OMP_THREADS unset  | OMP_NUM_THREADS={max_threads}/M
#           | SET_MKL_THREADS=M      | MKL_NUM_THREADS=M (M <= {max_threads})
# ----------------------------------------------------------------------
# D         | SET_OMP_THREADS=N      | OMP_NUM_THREADS=N
#           | SET_MKL_THREADS=M      | MKL_NUM_THREADS=M
#
# Note that in scenarios A, B, and C, the script goes to the trouble to check
# the machine is not oversubscribed. In all three of these scenarios, the user
# may not oversubscribe the machine, i.e. use more than ${max_threads}. In
# scenario D, the assumption is that since the user is explicitly setting both
# the SET_OMP_THREADS and SET_MKL_THREADS this is for good reason, and the user
# may oversubscribe the node with a warning.
#
# After a determination is made for SET_OMP_THREADS and SET_MKL_THREADS, these
# values are used to set OMP_NUM_THREADS and MKL_NUM_THREADS.
#
# The script then gives the user some details about the host node, the job, and
# the Singularity image it is making use of.
#
# Finally, the script will launch a Singularity container from a user provided
# image location or, if the 'default' keyword is given, from the latest image
# found by the `get_latest_image()` function in the "image_dir" directory.
#
# This script is dependent on being launched through qsub where UGE sets the
# $JOB_NAME, $JOB_ID, and $NSLOTS variables. These are used here, as well as
# specific machine name prefixes on the prod and lbd clusters which correspond
# to a certain hardcoded cores/slot value.
#
# This script is also dependent on the upstream R script that launches it to
# pass the ${sing_image} variable (qsub -v sing_image=...) with either the full
# path to the Singularity image to launch a container from or the 'default'
# keyword indicating to use the most current image. Optionally, the
# SET_OMP_THREADS and SET_MKL_THREADS values can be passed with the '-v' option
# in the qsub as well.
#
# -----------------------------------------------------------------------------
#
# Default directory containing LBD Singularity images
image_dir="/share/singularity-images/lbd"
#
# <---------------------------------------------------------------------------->
# <----------------------- Define Some Functions We Need ---------------------->
# <---------------------------------------------------------------------------->
#
# Use 'tr' to make a string all lower case to make our test easier
function make_lower_case() {
  echo $(echo $1 | tr '[:upper:]' '[:lower:]')
}

# Pull the vendor name out of '/proc/cpuinfo'
# Should be either "GenuineIntel" or "AuthenticAMD"
function get_cpu_vendor() {
  local vendor=$(cat /proc/cpuinfo | grep "vendor_id" | uniq | cut -d ':' -f 2)
  echo $(make_lower_case ${vendor})
}

# Unnecessary function to do float math with 'awk' because 'bc' was not
# originally installed on the lbd nodes when they were Ubuntu. It is now under
# CentOS, but no need to change it up now.
#
# Nice solution for rounding with 'awk' used below found here:
# http://bits.mdminhazulhaque.io/linux/round-number-in-bash-script.html
# Returns a rounded integer value of threads is careful not to round up to
# aggressively.  Will only round up if decimal is > 0.8 of a thread. If for some
# reason, the calculated number of threads exceed the number of cores available
# on the machine, we set the number of threads to the total number of cores on
# the machine.
function calc_threads() {
  local cores_per_slot=$1 # cores_per_slot
  local job_slots=$2      # slots obtained for the job
  local max_cores=$3      # total cores on the machine
  # calculate the raw number of cores based on slots and cores
  local raw_threads=$(echo ${cores_per_slot} ${job_slots} | awk '{print $1 * $2}')
  raw_threads=$(echo ${raw_threads} | awk '{print ($0-int($0)<0.799)?int($0):int($0)+1}')
  if [[ ${raw_threads} -le 0 ]]; then
    raw_threads=1
  elif [[ ${raw_threads} -gt ${max_cores} ]]; then
    raw_threads=${max_cores}
  fi
  echo "${raw_threads}"
}

# Some functions that I copied this from "sing_functs.sh" used in launching a
# Singularity container (or R within a Singularity container) for interactive
# use. I wanted to have a single, independent script in the lbd_core repo. These
# functions are simple enough to have repeated here.

# Function to determine if the requested thread value makes sense. Will throw
# errors for non-integer, negative, or zero values.
function check_threads() {
  # use regex to determine if the arg is an integer and exit if it is not
  if [[ ! $1 =~ ^[0-9]+$ ]] || [[ $1 -eq 0 ]]; then
    echo "ERROR: '$1' not an integer number of threads > 0 for $2."
    echo "Exiting..."
    exit 1
  fi
}

# Get the latest Singularity image file in the default LBD Singularity images
# directory.
function get_latest_image() {
  # Throw an error if we can't access the image directory
  if [[ ! -d "${image_dir}" ]]; then
    echo "ERROR: Could access image directory: '${image_dir}'"
    echo "Exiting..."
    exit 1
  fi

  # Use `ls()` to find the latest Singularity image (with *.simg extension)
  # Will give the complete path to the file
  local latest_image=$(ls -t "${image_dir}"/*.simg 2> /dev/null | head -1)
  if [[ -z "${latest_image}" ]]; then
    echo "ERROR: No *.simg images found in '${image_dir}'"
    echo "Exiting..."
    exit 1
  fi

  # Return the image name and complete path to file
  echo "${latest_image}"
}

# Echo's out some information on the Singularity image file that is nice for the
# user to know
function echo_sing_info() {
  # Expecting on a single argument
  if [[ $# -ne 1 ]]; then
    echo "ERROR: function 'echo_sing_info' expects the Singularity image name as the only argument...Exiting!"
    exit 1
  fi
  if [[ ! -f "${1}" ]]; then
    echo "ERROR: Can't access Singularity image: '${1}'"
    exit 1
  fi

  # Get the name of the Singularity image file only as well as the last
  # modification date.
  local latest_image_name=$(basename ${1})
  local latest_image_dir=$(dirname ${1})
  local image_creation_date=$(stat -c %y ${1} | cut -f1 -d ".")

  # Echo some details about the container we are attempting to launch and then
  # launch the container with Singularity run
  echo "Image name         : '${latest_image_name}'"
  echo "Image directory    : ${latest_image_dir}"
  echo "Creation date      : ${image_creation_date}"
  echo "Singularity version: $(singularity --version)"
}

# <---------------------------------------------------------------------------->
# <------------------- Determine threads for MKL and OpenMP ------------------->
# <---------------------------------------------------------------------------->
# Grab the hostname as a global since we use it all over, and error if it comes
# up empty since we need it to determine the number of slots
host=$(hostname -s)
if [[ -z "${host}" ]]; then
  echo "ERROR: Could not obtain host name"
  echo "Exiting..."
  exit 1
fi

# Throw an error if we can't find the "singularity" executable
if [[ -z $(command -v singularity) ]]; then
  echo "ERROR: Could not find 'singularity' executable."
  echo "Exiting..."
  exit 1
fi

# Make sure that /proc/cpuinfo exists, because we use it to determine how many
# cores are on the machine (faster than grepping this out of the `lscpu` output)
# and we also use it to determine the CPU vendor. If it exists use it to find
# the number of cores on the host machine
if [[ ! -f "/proc/cpuinfo" ]]; then
  echo "ERROR: no /proc/cpuinfo exists to find number of cores"
  echo "Exiting..."
  exit 1
else
  machine_cores=$(cat /proc/cpuinfo | grep "processor" | wc -l)
fi

# Make sure we got something for machine_cores and use `check_threads()` to make
# sure that the value make sense
if [[ -z $machine_cores ]]; then
  echo "ERROR: Was not able to determine number of cores on machine"
  echo "Exiting..."
  exit 1
elif [[ ! $machine_cores =~ ^[0-9]+$ ]] || [[ $machine_cores -eq 0 ]]; then
  echo "ERROR: Invalid value found for machine cores: ${machine_cores}"
  echo "Exiting..."
  exit 1
fi

# Determine the total slots the machine has been partitioned to in order to
# calculate cores/slot
machine_slots=$(qstat -f -l hostname=${host} | sed -n 3p | awk -F '( *|/)' '{print $5}')

# Use a default value if `qstat` gave us trouble
if [[ -z $machine_slots ]] || [[ ! $machine_slots =~ ^[0-9]+$ ]] || [[ $machine_slots -eq 0 ]]; then
  echo "----> WARNING: Could not determine machine slots from 'qstat'."
  # AMD nodes are either 100 or 51 slots, so being conservative if we can't find
  # the true total slot value.
  if [[ "${host}" == *"cn"* ]] && [[ "$(get_cpu_vendor)" == *"amd"* ]]; then
    machine_slots=100
    echo "---->          Using default of: ${machine_slots}"
  else
    machine_slots=${machine_cores}
    echo "---->          Using default of machine cores: ${machine_slots}"
  fi
fi

# Now that we know the total slots and the number of cores on the machine, we
# can calculate cores/slot:
cores_per_slot="$(echo ${machine_cores} ${machine_slots} | awk '{print $1 / $2}')"

# If this script was launched through `qsub` (as it should have been),
# $NSLOTS should have been set by UGE. If this is not set, exit, since
# it is sort of the whole point
if [[ -z "${NSLOTS}" ]]; then
  echo "ERROR: 'NSLOTS' environmental not set.  Did you qsub this script?"
  echo "Exiting..."
  exit 1
fi

# First, get the max threads based on $NSLOTS requested and machine:
# max_threads=$(get_threads ${machine_cores})
max_threads=$(calc_threads ${cores_per_slot} ${NSLOTS} ${machine_cores})

# Set OMP_NUM_THREADS and MKL_NUM_THREADS based on user settings and
# ${max_threads} calculated from slots and type of machine
#
# ------------------------------------------------------------------------------
# | Scenario A |
# --------------
# Both SET_OMP_THREADS and SET_MKL_THREADS are unset (default)
# ------------------------------------------------------------------------------
if [[ -z "${SET_OMP_THREADS}" ]] && [[ -z "${SET_MKL_THREADS}" ]]; then
  SET_OMP_THREADS=1
  SET_MKL_THREADS=${max_threads}

# ------------------------------------------------------------------------------
# | Scenario B |
# --------------
# SET_OMP_THREADS set but SET_MKL_THREADS is unset
# ------------------------------------------------------------------------------
elif [[ -n "${SET_OMP_THREADS}" ]] && [[ -z "${SET_MKL_THREADS}" ]]; then
  # Check to make sure that the requested thread value make sense.
  check_threads "${SET_OMP_THREADS}" "SET_OMP_THREADS"
  # Check for oversubscription
  if [[ "${SET_OMP_THREADS}" -gt "${max_threads}" ]]; then
    echo "----> WARNING: SET_OMP_THREADS requested > available cores. Setting as follows:"
    echo "---->          OMP_NUM_THREADS=${max_threads}"
    echo "---->          MKL_NUM_THREADS=1"
    SET_OMP_THREADS=${max_threads}
    SET_MKL_THREADS=1
  else
    SET_MKL_THREADS=$((${max_threads} / ${SET_OMP_THREADS}))
  fi

# ------------------------------------------------------------------------------
# | Scenario C |
# --------------
# SET_OMP_THREADS unset but SET_MKL_THREADS is set
# ------------------------------------------------------------------------------
elif [[ -z "${SET_OMP_THREADS}" ]] && [[ -n "${SET_MKL_THREADS}" ]]; then
  # Check to make sure that the requested thread value make sense.
  check_threads "${SET_MKL_THREADS}" "SET_MKL_THREADS"
  # Check for oversubscription
  if [[ "${SET_MKL_THREADS}" -gt "${max_threads}" ]]; then
    echo "----> WARNING: SET_MKL_THREADS requested > available cores. Setting as follows:"
    echo "---->          OMP_NUM_THREADS=1"
    echo "---->          MKL_NUM_THREADS=${max_threads}"
    SET_OMP_THREADS=1
    SET_MKL_THREADS=${max_threads}
  else
    SET_OMP_THREADS=$((${max_threads} / ${SET_MKL_THREADS}))
  fi

# ------------------------------------------------------------------------------
# | Scenario D |
# --------------
# BOTH SET_OMP_THREADS and SET_MKL_THREADS set
# ------------------------------------------------------------------------------
else
  # Check to make sure that the requested thread values make sense.
  check_threads "${SET_MKL_THREADS}" "SET_MKL_THREADS"
  check_threads "${SET_OMP_THREADS}" "SET_OMP_THREADS"
  # Check for oversubscription
  if [[ "$((${SET_OMP_THREADS} * ${SET_MKL_THREADS}))" -gt "${max_threads}" ]]; then
    echo "----> WARNING: SET_OMP_THREADS * SET_MKL_THREADS > available cores."
    echo "---->          [$((${SET_OMP_THREADS} * ${SET_MKL_THREADS})) > ${max_threads}]"
    echo "---->          You may be oversubscribing the node!"
  fi
fi

# At this point, if either SET_MKL_THREADS or SET_OMP_THREADS is empty, there's
# a problem
if [[ -z "${SET_OMP_THREADS}" ]] || [[ -z "${SET_MKL_THREADS}" ]]; then
  echo "ERROR: Either 'SET_OMP_THREADS' or 'SET_MKL_THREADS' empty:"
  echo "       SET_OMP_THREADS=${SET_OMP_THREADS}"
  echo "       SET_MKL_THREADS=${SET_MKL_THREADS}"
  echo "Exiting..."
  exit 1
else
  # If both are assigned, let's double check that the values make sense, since
  # calcs may have been done to determine them
  check_threads "${SET_MKL_THREADS}" "SET_MKL_THREADS"
  check_threads "${SET_OMP_THREADS}" "SET_OMP_THREADS"
fi

# <---------------------------------------------------------------------------->
# <----------------------------- Print User Info ------------------------------>
# <---------------------------------------------------------------------------->
echo "--------------------------------------------------------------------------------"
echo "Machine Info"
echo "----------------------------------------"
echo "Node               : ${host}"
echo "Total Cores        : ${machine_cores}"
echo "Total Slots        : ${machine_slots}"
echo -e "Cores/Slot         : ${cores_per_slot}\n"
echo "Job Info"
echo "----------------------------------------"
echo "Job Name           : ${JOB_NAME}"
echo "Job Id             : ${JOB_ID}"
echo "Execution Date     : $(date '+%c')"
echo "Slots              : ${NSLOTS}"
echo "OMP_NUM_THREADS    : ${SET_OMP_THREADS}"
echo -e "MKL_NUM_THREADS    : ${SET_MKL_THREADS}\n"
echo "Singularity Image Info"
echo "----------------------------------------"

# Make sure image is defined or the default is specified
if [[ -z "${sing_image}" ]]; then
  echo "ERROR: No Singularity image supplied"
  echo "Exiting..."
  exit 1
elif [[ "${sing_image}" == "default" ]]; then
  sing_image=$(get_latest_image)
  echo "Launching Singularity container from default image:"
else
  echo "Launching Singularity container from user specified image:"
fi
echo_sing_info "${sing_image}"
echo "--------------------------------------------------------------------------------"

# <---------------------------------------------------------------------------->
# <----------------------------- Launch Container ----------------------------->
# <---------------------------------------------------------------------------->
# Passing in the `umask` which we hopefully will not have to do after
# Singularity version > 2.5.1 is released.
# Can remove the `SINGULARITYENV_UMASK_ORIG=$(umask)` and
# `SINGULARITYENV_R_EXEC="/usr/local/bin/R < $1 --no-save $@"` variables and
# return to the simpler `singularity exec` after `umask` issue is resolved:
# singularity exec --cleanenv "${sing_image}" /usr/local/bin/R <$1 --no-save $@
#
SINGULARITYENV_R_EXEC="/usr/local/bin/R < $1 --no-save $@"        \
SINGULARITYENV_UMASK_ORIG=$(umask)                                \
SINGULARITYENV_OMP_NUM_THREADS=${SET_OMP_THREADS}                 \
SINGULARITYENV_MKL_NUM_THREADS=${SET_MKL_THREADS}                 \
SINGULARITYENV_OMP_NESTED=true                                    \
SINGULARITYENV_MKL_DYNAMIC=false                                  \
singularity exec --cleanenv "${sing_image}" /bin/sh -c            \
                 'umask "${UMASK_ORIG}" && eval ${R_EXEC}'
