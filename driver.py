import sys
from parsefile import *

err_flag = 1

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
    statements_whole = text.split()
   
    print(statements)
    print(statements_whole)

    if len(statements) == len(statements_whole):
        err_flag = 0

    # Remove leading and trailing whitespace from each line
    statements = [statement.strip() + ";" for statement in statements]
    mystring = ''

    for x in statements:
        x.replace(' \n', '')
        x.replace(' ','')
        mystring += x

    with open('NOSPACES.txt', 'w') as file:
    # Write the single line to the file
        file.write(mystring)

    #print(statements)
    if statements:
        print(statements)
        statements.pop()

        for statement in statements:
            #print(statement)
            characters = statement.strip()  # Read and strip each line
            if not characters:
                continue  # Skip empty lines
            tokens = imp_lex(characters)
            #print(tokens)
            parser = globals()["stmt"]()
            result = parser(tokens, 0)
            print(result)