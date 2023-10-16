import interpreter as basic

while True:
    text = input('basic > ')
    if text.strip() == "": continue
    result, error = basic.run('<stdin>',text)

    if error: print(error.as_string())
    elif result: 
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
            pass
        #print(repr(result))