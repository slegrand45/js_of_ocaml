#!/bin/bash

# Couleurs pour la sortie
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # Pas de couleur

# Chemins relatifs
JSOO="${JSOO:-../../_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe}"
RUNTIME_PHP="${RUNTIME_PHP:-../../runtime/php/runtime.php}"

# Préparation de l'environnement
cd "$(dirname "$0")"
mkdir -p tmp_out
cp -f "$RUNTIME_PHP" ./runtime.php

# Vérification du compilateur
if [ ! -f "$JSOO" ]; then
    echo -e "${YELLOW}Compilateur non trouvé. Tentative de construction...${NC}"
    (cd ../.. && dune build compiler/bin-js_of_ocaml/js_of_ocaml.exe)
fi

TOTAL=0
PASSED=0
FAILED=()

echo "======================================================"
echo "   Lancement de la suite de tests OCaml -> PHP        "
echo "======================================================"

for f in *.ml; do
    [ -e "$f" ] || continue
    TOTAL=$((TOTAL + 1))
    test_name="${f%.ml}"
    
    printf "Testing %-30s " "${test_name}..."

    # 1. Sortie de référence (OCaml Bytecode)
    ocamlc -o "tmp_out/${test_name}.byte" "$f" 2>/dev/null
    timeout 10s ocamlrun "tmp_out/${test_name}.byte" > "tmp_out/${test_name}.ocaml.out" 2>&1
    
    # 2. Sortie PHP (Génération via ocamlephan)
    # Le compilateur injecte maintenant <?php en tête de fichier
    extra_args=""
    if [[ "$test_name" == *"effect"* ]]; then
        extra_args="--effects=fibers"
    fi
    "$JSOO" --php --pretty --noruntime $extra_args "tmp_out/${test_name}.byte" -o "tmp_out/${test_name}.php" 2>/dev/null

    # On ajoute simplement le require_once pour le runtime (sans <?php car déjà présent)
    echo "<?php require_once('./runtime.php');" > "tmp_out/final_${test_name}.php"
    # On saute la première ligne du fichier généré (<?php) pour éviter la duplication
    tail -n +2 "tmp_out/${test_name}.php" >> "tmp_out/final_${test_name}.php"
    
    # Exécution PHP
    timeout 10s bash -c "php \"tmp_out/final_${test_name}.php\" | grep -v '^//' | grep -v '^#'" > "tmp_out/${test_name}.php.out" 2>&1
    
    # Vérification si timeout a tué le processus (code de sortie 124)
    if [ $? -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC}"
        echo "Error: Test timed out after 10s" > "tmp_out/${test_name}.php.out"
        FAILED+=("$test_name (TIMEOUT)")
        continue
    fi
    
    # 3. Comparaison
    if diff "tmp_out/${test_name}.ocaml.out" "tmp_out/${test_name}.php.out" > /dev/null; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED+=("$test_name")
    fi
done

echo "======================================================"
echo -e "Résultat : ${GREEN}$PASSED${NC} / $TOTAL tests réussis."

if [ ${#FAILED[@]} -ne 0 ]; then
    echo -e "${RED}Tests échoués :${NC}"
    for t in "${FAILED[@]}"; do
        echo -e "  - $t (consulter tmp_out/${t}.php.out pour plus de détails)"
    done
    exit 1
fi

exit 0
