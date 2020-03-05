#reminder: source this file in ~/.bashrc

if [[ $OSTYPE =~ "darwin" ]]; then
   echo "Activating Darwin Setup"
   source ~/.shell_files/exports.bash
   source ~/.shell_files/languageSpecific.bash
   source ~/.shell_files/aliases.bash
   set_non_standard_python_paths
   echo "Activating base conda"
   source activate base
fi
