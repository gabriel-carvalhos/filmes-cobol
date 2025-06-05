# Filmes COBOL 🎬

https://github.com/user-attachments/assets/f61489b1-f718-4b3f-8f09-8a5eda56e9a3

Este é um sistema de gerenciamento de filmes desenvolvido em **COBOL**, com interface estruturada em tela.

O projeto foi realizado ao final do bootcamp de COBOL da **Código de Base**, com orientação do instrutor Ivan.


## 📚 Funcionalidades

- Inclusão de novos filmes
- Consulta de filmes por código
- Alteração de informações de filmes existentes
- Exclusão de filmes do sistema
- Geração de relatório completo de filmes
- Validação de dados de entrada
- Interface utilizando `SCREEN SECTION`, com mensagens de erro formatadas
- Armazenamento em arquivos indexados
- Estrutura monolítica

## 💻 Setup

### Pré-Requisitos

- GnuCOBOL 3.x ou superior
- Terminal compatível com entrada/saída de tela (para exibir `SCREEN SECTION`)

### Como executar

1. **Compile o código COBOL**

   Utilize um compilador como o GnuCOBOL:

   ```bash
   cobc -x filmes.cbl -o filmes
   ```

2. **Execute o programa**

   ```bash
   ./filmes
   ```
