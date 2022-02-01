# reminder: source this file in ~/.bash_profile
# and source .bash_profile in .bashrc

if [[ $OSTYPE =~ "darwin" ]]; then
   echo "Activating Darwin Setup"
   # This is how to activate conda if it
   # complains the shell isn't set up:
   # source ~/anaconda3/etc/profile.d/conda.sh
   #
   source ~/.shell_files/bash_setup/base_path.bash
   for fname in $(find ~/.shell_files/bash_setup -type f -name "*.bash" -not -name "exports.bash" -not -name "base_path.bash")
   do
       source $fname
   done
   set_non_standard_python_paths
   source ~/.shell_files/bash_setup/exports.bash
   # Auto Activate an environment if necessary:
   if test -a ".venv";
   then
       ENV=$(tail -n 1 .venv)
       echo "Conda: ${ENV}"
       conda activate $ENV
   else
    echo "Conda: ${OSTYPE}"
	conda activate base310
   fi
fi
