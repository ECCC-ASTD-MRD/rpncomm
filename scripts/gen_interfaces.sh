#! /usr/bin/env bash
# set -x

#SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

function extract_lines() {
  target=${1}
  criterion=${2}
  if grep -q ${criterion} ${target}; then
    base=${target##*/}
    grep ${criterion} ${target} | sed \
      -e 's/^[\t ]*!!//' \
      -e 's/[ ]*!.*//' \
      -e 's/[ ]*$//' \
      -e 's/^[ \t]*/    /' \
      -e 's/^\s*#/#/'
    echo
  fi
}

TMP_FILE=rpn_comm_itf.tmp
rm -f ${TMP_FILE}

echo "!"
echo "! This file is generated at compile time. Do not modify it"
echo "!"

echo
echo "#if 0"
for target in ${@}; do
  extract_lines ${target} '!InTfX!'
done
echo "#endif"
echo
echo "  interface"
for target in ${@}; do 
  extract_lines ${target} '!InTf!'
  extract_lines ${target} '!InTfout!' >> ${TMP_FILE}
done
echo "  end interface"

echo "! PREPROCESSOR INTERFACES"
echo "  interface"
cat ${TMP_FILE}
echo "  end interface"

rm -f ${TMP_FILE}

