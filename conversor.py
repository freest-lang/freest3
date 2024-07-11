import chardet

input_file_path = 'run.log'
output_file_path = 'run_utf8.log'

# Detectar o encoding original do ficheiro
with open(input_file_path, 'rb') as rawdata:
    result = chardet.detect(rawdata.read(10000))  # Lê os primeiros 10000 bytes para detectar o encoding

original_encoding = result['encoding']
print(f"Encoding detectado: {original_encoding}")

if original_encoding is not None:
    # Ler o ficheiro no encoding detectado
    with open(input_file_path, 'r', encoding=original_encoding) as input_file:
        content = input_file.read()

    # Gravar o conteúdo no novo ficheiro em UTF-8
    with open(output_file_path, 'w', encoding='utf-8') as output_file:
        output_file.write(content)

    print(f"Ficheiro convertido e gravado em UTF-8 como {output_file_path}")
else:
    print("Não foi possível detectar o encoding do ficheiro.")
