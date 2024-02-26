#!/bin/bash

directory="example/valid/IO/print"

for file in $(find "$directory" -type f); do
    if [ -f "$file" ]; then

        echo "Checking $file\n"

        filename=$(basename "$file")
        exec_name="${filename%.*}"
        file_content=$(cat $file)
        expected_output=$(echo "$file_content" | sed -n '/Output:/,/Program:/p' | sed '/Output:/d; /Program:/d' | sed -n '/Exit:/q;p' | sed 's/^# //g' | sed 's/^#//g')

        echo "Compiling...\n"
        if cabal run wacc25 -- "$file" --no-text-deco; then
            echo "\nCompilation of $filename succeeded!"
            echo "\nExecuting..."
            
            if gcc -o "$exec_name" -z noexecstack "$exec_name.S"; then
                echo "\nExecution succeeded!"
                output=$(./"$exec_name")

                echo "The expected output is: $expected_output"
                echo "The actual output is: $output"
                if [ "$expected_output" = "$output" ]; then
                    echo "The output is correct!"
                else
                    echo "The output is wrong!"
                fi
                rm $exec_name
            else
                echo "\nExecution failed!"
            fi
        else
            echo "\nCompilation of $filename failed!"
        fi

        echo "\n\n"

    fi
done

rm *.S
