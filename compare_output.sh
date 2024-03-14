#!/bin/bash

directory="example/valid/"
total=0
passed=0
failed_files=""

for file in $(find "$directory" -type f \
    -not -path "example/valid/advanced/*"); do
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
        expected_exit_code=$(echo "$file_content" \
            | sed -n '/Exit:/,/begin/p' \
            | sed '/begin/d' \
            | sed -n '/Exit:/,/Program:/p' \
            | sed '/Exit:/d; /Program:/d' \
            | sed 's/^# //g')
        expected_output=$(echo "$file_content" \
            | sed -n '/Output:/,/begin/p' \
            | sed '/begin/d' \
            | sed -n '/Output:/,/Program:/p' \
            | sed '/Output:/d; /Program:/d' \
            | sed -n '/Exit:/q;p' \
            | sed 's/^# //g' \
            | sed 's/^#//g' \
            | sed 's/^addrs#/#addrs#/g')

        echo -e "Compiling...\n"
        if ./compile "$file"; then
            rm "$exec_name.macro.S"
            echo -e "\nCompilation of $filename succeeded!"
            echo -e "\nExecuting..."
            if [ -n "$input" ]; then
                echo -e "Program input is: $input"
            fi

            if gcc -o "$exec_name" -z noexecstack "$exec_name.s"; then
                output=$(./"$exec_name" <<< $input)
                exit_code=$?
                echo -e "\nExecution succeeded!\n"

                if [ -n "$expected_exit_code" ]; then
                    echo -e "The expected exit code is:\n$expected_exit_code"
                    echo -e "The actual exit code is:\n$exit_code"
                    if [ "$expected_exit_code" = "$exit_code" ]; then
                        echo -e "The exit code is correct!\n"
                    else
                        echo -e "The exit code is wrong!\n\n"
                        failed_files="$failed_files\n$file"
                        rm "$exec_name"
                        rm "$exec_name.s"
                        continue
                    fi
                fi

                if [[ "$file" != example/valid/runtimeErr/* ]]; then
                    echo -e "The expected output is:\n$expected_output"
                    echo -e "The actual output is:\n$output"
                    output=$(echo "$output" \
                        | sed 's/0x[0-9a-f]\{12\}/#addrs#/g')
                    if [ "$expected_output" = "$output" ]; then
                        echo "The output is correct!"
                        ((passed++))
                    else
                        echo "The output is wrong!"
                        failed_files="$failed_files\n$file"
                    fi
                else
                    ((passed++))
                fi
                rm "$exec_name"
            else
                echo -e "\nExecution failed!"
                failed_files="$failed_files\n$file"
            fi

            rm "$exec_name.s"
        else
            echo -e "\nCompilation of $filename failed!"
            failed_files="$failed_files\n$file"
        fi

        echo -e "\n\n"
    fi
done

echo "$passed/$total passed!"

if [ $passed -eq $total ]; then
    exit 0
else
    echo -e "\nFailed files: $failed_files"
    exit 1
fi
