# Game NES
Base de um jogo para NES.

# Compilando o projeto
Adicione CL65 como uma variável de ambiente.
```sh
export CL65=path/to/cl65
```

Execute o script de build
```sh
make
```

Pronto, agora você tem a rom `main.nes`.

# Compilando e executando a rom
Instale o emulador de NES ![fceux](https://fceux.com/web/home.html).

Execute o script `run`.
```sh
make run
```

Pronto, agora a rom será compilada e executada pelo fceux.
