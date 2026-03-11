#!/bin/bash
# Utilisation : ./test_compiler.sh mon_test.ml

SOURCE_ML=$1
BYTECODE_EXE="${SOURCE_ML%.ml}.byte"
OUTPUT_PHP="${SOURCE_ML%.ml}.php"

echo "--- 1. Compilation OCaml (Bytecode) ---"
ocamlc -g -o "$BYTECODE_EXE" "$SOURCE_ML"

echo "--- 2. Passage dans ocamlephan (ton JSOO forké) ---"
# On appelle ton binaire jsoo modifié qui génère maintenant du PHP
../../_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe --php --pretty --noruntime "$BYTECODE_EXE" -o "$OUTPUT_PHP"

echo "--- 3. Post-processing (Injection du Runtime Bridge) ---"
# Copie du runtime
cp -v -f ../../runtime/php/runtime.php ./
# On insère le runtime_bridge.php au début du fichier généré
echo "<?php require_once('./runtime.php'); ?>" > "final_$OUTPUT_PHP"
cat "$OUTPUT_PHP" >> "final_$OUTPUT_PHP"

echo "--- 4. Exécution PHP ---"
php "final_$OUTPUT_PHP"
