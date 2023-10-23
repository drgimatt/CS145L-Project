import sys
from parsefile import *

err_flag = 0

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write('Usage: %s filename' % sys.argv[0])
        sys.exit(1)
    filename = sys.argv[1]
    with open(filename, 'r') as file:
        text = file.read()
    file.close()

    # Split the text into lines using semicolons as indicators
    statements = text.split(';')
    # Remove leading and trailing whitespace from each line
    statements = [statement.strip() + ";" for statement in statements]

    statements = [item for item in statements if item.strip() != ';']

    if statements:
        
        no_spaces = ''.join(statements)
        no_spaces = no_spaces.replace('\n', '').replace(' ', '')

        with open('NOSPACES.txt', 'w') as file:
            file.write(no_spaces)

        file.close()

        for statement in statements:

            characters = statement.strip()  # Read and strip each line
            if not characters:
                continue  # Skip empty lines

            tokens = imp_lex(characters)

            condition = "stmt"

            function_mapping = {"stmt": stmt, "bexp" : bexp, "aexp" : aexp}

            for condition in function_mapping:
                parser = function_mapping[condition]()
                result = parser(tokens, 0)
                if result:
                    #print(result)
                    break
                else:
                    err_flag = 1


    if err_flag == 0:
        print("NO ERROR(S) FOUND")

        with open("NOSPACES.TXT", 'r') as file:
            content = file.read()
            
        file.close()

        reserved_pattern = {'integer', 'double', 'if', 'output','else', 'while', 'do', 'end', 'and', 'or', 'not'}
        symbols_pattern = set(': ; := ( < ) << + - * / <= > >= == !=')

        # Extract symbols from the content
        extracted_reserved = []
        current_word = ''

        for char in content:
            if char.isalnum() or char == '_':
                current_word += char
            else:
                if current_word in reserved_pattern:
                    extracted_reserved.append(current_word)
                current_word = ''

        extracted_symbols = ''.join(
            [char if char in symbols_pattern else ' ' for char in content])

        # Remove consecutive spaces and split into a list of symbols
        reserved_string = ' '.join(extracted_reserved)
        symbol_string = ' '.join(extracted_symbols.split())

        with open('RES_SYM.TXT', 'w') as file:
            file.write("Reserved Words: " + reserved_string)
            file.write('\n')
            file.write("Symbols: " + symbol_string)

        file.close()

    else:
        print("ERROR")
        sys.exit(1)
