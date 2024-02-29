#!/bin/bash

directory="example/valid/"
total=0
passed=0

for file in $(find "$directory" -type f \
    ! \( -path "example/valid/advanced/hashTable.wacc" \
    -o -path "example/valid/advanced/ticTacToe.wacc" \)); do
    if [ -f "$file" ]; then

        ((total++))
        echo -e "Checking $file\n"

        filename=$(basename "$file")
        exec_name="${filename%.*}"
        file_content=$(cat $file)
        input=$(echo "$file_content" \
            | sed -n '/Input:/,/Output:/p' \
            | sed '/Output:/d' \
            | sed 's/^# Input: //g')
        expected_output=$(echo "$file_content" \
            | sed -n '/Output:/,/begin/p' \
            | sed '/begin/d' \
            | sed -n '/Output:/,/Program:/p' \
            | sed '/Output:/d; /Program:/d' \
            | sed -n '/Exit:/q;p' \
            | sed 's/^# //g' \
            | sed 's/^#//g')

        echo -e "Compiling...\n"
        if cabal run wacc25 -- "$file" --no-text-deco; then
            echo -e "\nCompilation of $filename succeeded!"
            echo -e "\nExecuting..."
            if [ -n "$input" ]; then
                echo -e "Program input is: $input"
            fi

            if gcc -o "$exec_name" -z noexecstack "$exec_name.s"; then
                output=$(./"$exec_name" <<< $input)
                echo -e "\nExecution succeeded!\n"

                echo -e "The expected output is:\n$expected_output"
                echo -e "The actual output is:\n$output"
                if [ "$expected_output" = "$output" ]; then
                    echo "The output is correct!"
                    ((passed++))
                else
                    echo "The output is wrong!"
                fi
                rm $exec_name
            else
                echo -e "\nExecution failed!"
            fi

            rm "$exec_name.s"
        else
            echo -e "\nCompilation of $filename failed!"
        fi

        echo -e "\n\n"
    fi
done

echo "$passed/$total passed!"

if [ $passed -eq $total ]; then
    exit 0
else
    exit 1
fi
