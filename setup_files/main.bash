# reminder: source this file in ~/.bash_profile
# and source .bash_profile in .bashrc

if [[ $OSTYPE =~ "darwin" ]]; then
   echo "Activating Darwin Setup"
   source ~/.shell_files/exports.bash
   source ~/.shell_files/languageSpecific.bash
   source ~/.shell_files/aliases.bash
   set_non_standard_python_paths
   # Auto Activate an environment if necessary:
   if test -a ".venv";
   then
       ENV=$(tail -n 1 .venv)
       echo "Conda: ${ENV}"
       conda activate $ENV
   else
    echo "Conda: ${OSTYPE}"
	conda activate base
   fi
fi
