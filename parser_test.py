import sys
from parsefile import *

if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.stderr.write('usage: %s filename parsername' % sys.argv[0])
        sys.exit(1)
    filename = sys.argv[1]
    with open(filename, 'r') as file:
        text = file.read()
    file.close()

    # Split the text into lines using semicolons as indicators
    statements = text.split(';')

    # Remove leading and trailing whitespace from each line
    statements = [statement.strip() + ";" for statement in statements]

    if statements:
        statements.pop()

    if statements:
        for statement in statements:
            print(statement)
            characters = statement.strip()  # Read and strip each line
            if not characters:
                continue  # Skip empty lines
            tokens = imp_lex(characters)
            print(tokens)
            parser = globals()[sys.argv[2]]()
            result = parser(tokens, 0)
            if result:
                print(result)
            else:
                print("Statement not parseable")
    else:
        print("No result found")