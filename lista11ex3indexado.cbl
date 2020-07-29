      $set sourceformat"free"
      *>Divisão de Identificação do Programa
       identification division.
       program-id. "lista11ex3indexado".
       author. "Elaine Martina André".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.

      *>Divisão Para Configuração do Ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>------------------------------------------------------------------------
      *>-----Declaração dos Recursos Externos
       input-output section.
       file-control.

           select arqCadAlunosIndex assign to "arqCadAlunosIndex.dat"   *> Select - Add o Nome do Arquivo e Assign - Associa o Arquivo Fisico
           organization is indexed                                      *> Forma de Organização Dos Dados
           access mode is dynamic                                       *> Acess - Como Vou Acessar os Dados
           lock mode is automatic                                       *> Para Mais de Um Usuario Usar ao Mesmo Tempo Sem Perder Dados e Sem Ficar Lento
           record key is fd-cod-aluno                                   *> Chave
           file status is ws-fs-arqCadAlunosIndex.                      *> File Status- Status da Ultima Operação

       i-o-control.

      *>------------------------------------------------------------------------
      *>Declaração de Variáveis
       data division.

      *>----Variaveis de Arquivos
       file section.
       fd arqCadAlunosIndex.  *> Inicio da Declração das Variaveis do Arquivo
       01  fd-alunos.
           05  fd-cod-aluno                        pic 9(03).
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-notas.
               10  fd-nota1                        pic 9(02)v99.
               10  fd-nota2                        pic 9(02)v99.
               10  fd-nota3                        pic 9(02)v99.
               10  fd-nota4                        pic 9(02)v99.

      *>------------------------------------------------------------------------
      *>----Variaveis de Trabalho
       working-storage section.
       77  ws-fs-arqCadAlunosIndex                 pic  9(02).

       01  ws-alunos.
           05  ws-cod-aluno                        pic 9(03).
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-notas.
               10  ws-nota1                        pic 9(02)v99.
               10  ws-nota2                        pic 9(02)v99.
               10  ws-nota3                        pic 9(02)v99.
               10  ws-nota4                        pic 9(02)v99.

       77  ws-menu                                 pic x(02).
       77  ws-aux                                  pic x(01).

       77  ws-sair                                 pic  x(01).
          88  ws-fechar-programa                   value "S" "s".
          88  ws-voltar-tela                       value "V" "v".

       01 ws-msn-erro.
           05 ws-msn-erro-ofsset                   pic 9(04).
           05 filler                               pic x(01) value "-".
           05 ws-msn-erro-cod                      pic 9(02).
           05 filler                               pic x(01) value space.
           05 ws-msn-erro-text                     pic x(42).
      *>------------------------------------------------------------------------

      *>----Variaveis Para Comunicação Entre Programas
       linkage section.

      *>----Declaração de Tela
       screen section.

      *>------------------------------------------------------------------------
      *>Declaração do Corpo do Programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>                     Procedimentos de Inicialização
      *>------------------------------------------------------------------------
       inicializa section.

      *>   Open i-o - Abre o Arquivo Para Leitura e Escrita
           open i-o arqCadAlunosIndex
      *>   Tratamento de Erro - Caso O File Status dê Diferente de Zero (Comando Executado com Sucesso) e Cinco Aparecerá a Mensagem de Erro na Section Finaliza Anormal
           if ws-fs-arqCadAlunosIndex  <> 00
           and ws-fs-arqCadAlunosIndex <> 05 then
               move 1                                          to ws-msn-erro-ofsset
               move ws-fs-arqCadAlunosIndex                    to ws-msn-erro-cod
               move "Erro ao Abrir arq. arqCadAlunosIndex "    to ws-msn-erro-text
               perform finaliza-anormal
           end-if

      *>   Inicialização do Menu
           move  spaces    to     ws-menu

           .
       inicializa-exit.
           exit.
      *>------------------------------------------------------------------------
      *>                         Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until ws-fechar-programa
               move space to ws-sair

      *>       Menu do Programa
               display "Insira '1' Para Cadastrar Aluno"
               display "Insira '2' Para Cadastrar Notas"
               display "Insira '3' Para Consulta Indexada"
               display "Insira '4' Para Consulta Sequencial"
               display "Insira '5' Para Deletar"
               display "Insira '6' Para Alterar"
               display "Insira 'S' Para Sair"
               accept ws-menu

      *>       Escolha das Opções do Menu do Programa
               evaluate  ws-menu
       *>          Cadastrar Aluno
                   when = '1'
                       perform cadastrar-aluno
       *>          Cadastrar Notas
                   when = '2'
                       perform cadastrar-notas
       *>          Fazer Consulta Indexada (Apartir do Ponto Informado)
                   when = '3'
                       perform consulta-indexada
       *>          Fazer Consulta Sequencial de 1 em 1 do Primeiro ao Ultimo ou do Ultimo ao Primeiro
                   when = '4'
                       perform consulta-sequencial-next
       *>          Deletar Cadastro Informado
                   when = '5'
                       perform deletar-cadastro
       *>          Alterar Cadastro Informado
                   when = '6'
                       perform alterar-cadastro
       *>          Fechar o Programa
                   when = "S"
                       perform finaliza
       *>          Caso a Entrada Inserida Não Seja Nenhuma das Opções do Menu
                   when other
                       display "Opcao Invalida!"
               end-evaluate

           end-perform

      *>   O Aceite Abaixo Serve Unicamente Para Manter a Tela Parada
           accept ws-aux

           .
       processamento-exit.
           exit.
      *>------------------------------------------------------------------------
      *>                          Cadastro de Alunos
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

           perform until ws-voltar-tela

               display erase
               display "-------  Cadastro de Alunos -------"
      *>       Cadastrando os Dados dos Alunos
               display "Codigo do Aluno: "
               accept ws-cod-aluno

               display "Nome do Aluno  : "
               accept ws-aluno

               display "Endereco       : "
               accept ws-endereco

               display "Nome da Mae    : "
               accept ws-mae

               display "Nome do Pai    : "
               accept ws-pai

               display "Telefone       : "
               accept ws-telefone

      *>       Escrever Dados no Arquivo
               write fd-alunos from ws-alunos
      *>       Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro na Section Finaliza Anormal
               if ws-fs-arqCadAlunosIndex  <> 0 then
                   move 2                                           to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunosIndex                     to ws-msn-erro-cod
                   move "Erro ao Gravar arq. arqCadAlunosIndex "    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

      *>       Condição de Saida
               display " "
               display "Deseja Cadastrar Mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       cadastrar-aluno-exit.
           exit.
      *>------------------------------------------------------------------------
      *>                          Cadastro de Notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           perform until ws-voltar-tela

               display erase
               display "------ Cadastro de Notas ------"
      *>       Identificando o Aluno
               display "Informe o Codigo do Aluno: "
               accept ws-cod-aluno

      *>       Informando as Notas do Aluno
               display "Informe a Primeira Nota: "
               accept ws-nota1

               display "Informe a Segunda Nota : "
               accept ws-nota2

               display "Informe a Terceira Nota: "
               accept ws-nota3

               display "Informe a Quarta Nota  : "
               accept ws-nota4

      *>       Salvando as Notas no Arquivo
               move ws-cod-aluno to fd-cod-aluno *> Preenche a Chave
               read arqCadAlunosIndex            *> Le o Arquivo
      *>       Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro na Section Finaliza Anormal
               if ws-fs-arqCadAlunosIndex  <> 00 then
      *>           Tratamento de Erro - Caso O File Status dê igual 23 o Registro Não Existe (Nesse Caso o Codigo)
                   if ws-fs-arqCadAlunosIndex = 23 then
                       display "Codigo de Aluno Inexistente"
                   else
                       move 3                                        to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlunosIndex                  to ws-msn-erro-cod
                       move "Erro ao Ler arq. arqCadAlunosIndex "    to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
                   move ws-notas to fd-notas
      *>           Reescrevendo Dados
                   rewrite fd-alunos
      *>           Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro na Section Finaliza Anormal
                   if ws-fs-arqCadAlunosIndex  <> 00 then
                       move 4                                          to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlunosIndex                    to ws-msn-erro-cod
                       move "Erro ao Gravar arq. arqCadAlunosIndex "   to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

      *>       Condição de Saida
               display " "
               display "Deseja Cadastrar Mais Alguma Nota? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       cadastrar-notas-exit.
           exit.
      *>------------------------------------------------------------------------
      *>           Rotina de Consulta - Lê o Arquivo de Forma Indexada
      *>------------------------------------------------------------------------
       consulta-indexada section.

           display erase
      *>   Identificando o Aluno
           display "Informe o Codigo do Aluno a Ser Consultado:"
           accept ws-cod-aluno

           move ws-cod-aluno to fd-cod-aluno
      *>   Ler Dados do Arquivo
           read arqCadAlunosIndex
      *>   Tratamento de Erro - Caso O File Status dê Diferente de Zero e Dez Aparecerá a Mensagem de Erro na Section Finaliza Anormal
           if  ws-fs-arqCadAlunosIndex <> 0
           and ws-fs-arqCadAlunosIndex <> 10 then
      *>       Tratamento de Erro - Caso O File Status dê igual 23 o Registro Não Existe (Nesse Caso o Codigo)
               if ws-fs-arqCadAlunosIndex = 23 then
                   display "Codigo Informado Invalido!"
               else
                   move 5                                         to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunosIndex                   to ws-msn-erro-cod
                   move "Erro ao ler arq. arqCadAlunosIndex "     to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           move  fd-alunos       to  ws-alunos

      *>   Displays na Tela
           display "Codigo       : " ws-cod-aluno
           display "Aluno        : " ws-aluno
           display "Endereco     : " ws-endereco
           display "Nome da Mae  : " ws-mae
           display "Nome do Pai  : " ws-pai
           display "Telefone     : " ws-telefone
           display " "
           display "Primeira Nota: " ws-nota1
           display "Segunda Nota : " ws-nota2
           display "Terceira Nota: " ws-nota3
           display "Quarta Nota  : " ws-nota4

           .
       consulta-indexada-exit.
           exit.
      *>------------------------------------------------------------------------
      *>      Rotina de Consulta - Lê o Arquivo de Forma Sequencial Next
      *>------------------------------------------------------------------------
       consulta-sequencial-next section.

           perform until ws-voltar-tela

               display erase
      *>       Ler Dados do Arquivo
               read arqCadAlunosIndex next
      *>       Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro na Section Finaliza Anormal
               if  ws-fs-arqCadAlunosIndex <> 0  then
      *>          Tratamento de Erro - Caso O File Status dê Igual a 10 Irá Para a Section de Consulta Sequencial Prev
                  if ws-fs-arqCadAlunosIndex = 10 then
                      perform consultar-sequencial-prev
                  else
                      move 6                                       to ws-msn-erro-ofsset
                      move ws-fs-arqCadAlunosIndex                 to ws-msn-erro-cod
                      move "Erro ao Ler arq. arqCadAlunosIndex "   to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-alunos       to  ws-alunos

      *>       Displays na Tela
               display "Codigo       : " ws-cod-aluno
               display "Aluno        : " ws-aluno
               display "Endereco     : " ws-endereco
               display "Nome da Mae  : " ws-mae
               display "Nome do Pai  : " ws-pai
               display "Telefone     : " ws-telefone
               display " "
               display "Primeira Nota: " ws-nota1
               display "Segunda Nota : " ws-nota2
               display "Terceira Nota: " ws-nota3
               display "Quarta Nota  : " ws-nota4

      *>       Condição de Saida
               display "Deseja Consultar o Proximo Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       consulta-sequencial-next-exit.
           exit.
      *>------------------------------------------------------------------------
      *>      Rotina de Consulta - Lê o Arquivo de Forma Sequencial Prev
      *>------------------------------------------------------------------------
       consultar-sequencial-prev section.

           perform until ws-voltar-tela

               display erase
      *>       Ler Dados do Arquivo
               read arqCadAlunosIndex previous
      *>       Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro na Section Finaliza Anormal
               if  ws-fs-arqCadAlunosIndex <> 0  then
      *>          Tratamento de Erro - Caso O File Status dê Igual a 10 Irá Para a Section de Consulta Sequencial Next
                  if ws-fs-arqCadAlunosIndex = 10 then
                      perform consulta-sequencial-next
                  else
                      move 7                                       to ws-msn-erro-ofsset
                      move ws-fs-arqCadAlunosIndex                 to ws-msn-erro-cod
                      move "Erro ao Ler arq. arqCadAlunosIndex "   to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-alunos       to  ws-alunos

      *>       Displays na Tela
               display "Codigo       : " ws-cod-aluno
               display "Aluno        : " ws-aluno
               display "Endereco     : " ws-endereco
               display "Nome da Mae  : " ws-mae
               display "Nome do Pai  : " ws-pai
               display "Telefone     : " ws-telefone
               display " "
               display "Primeira Nota: " ws-nota1
               display "Segunda Nota : " ws-nota2
               display "Terceira Nota: " ws-nota3
               display "Quarta Nota  : " ws-nota4

      *>       Condição de Saida
               display "Deseja Consultar o Aluno Anterior? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       consultar-sequencial-prev-exit.
           exit.

      *>------------------------------------------------------------------------
      *>                  Rotina de Apagar / Delete
      *>------------------------------------------------------------------------
       deletar-cadastro section.

           display erase
      *>   Identificando o Aluno
           display "Informe o Codigo do Aluno a Ser Excluido:"
           accept ws-cod-aluno

           move ws-cod-aluno to fd-cod-aluno
      *>   Apagar Dados do Registro do Arquivo
           delete arqCadAlunosIndex
      *>   Tratamento de Erro - Caso O File Status dê Igual Zero a Informação Será Deletada com Sucesso
           if  ws-fs-arqCadAlunosIndex = 0 then
               display "Aluno de Codigo " ws-cod-aluno " Deletado Com Sucesso!"
           else
      *>       Tratamento de Erro - Caso O File Status dê igual 23 o Registro Não Existe (Nesse Caso o Codigo)
               if ws-fs-arqCadAlunosIndex = 23 then
                   display "Codigo Informado Invalido!"
               else
                   move 8                                      to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunosIndex                to ws-msn-erro-cod
                   move "Erro ao Ler arq. arqCadAlunosIndex "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           .
       deletar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Rotina de alteração de temperatura
      *>------------------------------------------------------------------------
       alterar-cadastro section.

           display erase
      *>   Identificando o Aluno
           display "Informe o Codigo do Aluno a Ser Alterado:"
           accept ws-cod-aluno

           display "Altere o Cadastro Informando Todos os Dados: "

      *>   Alterando os Dados do Cadastro
           display "Aluno      : "
           accept  ws-aluno

           display "Endereco   : "
           accept ws-endereco

           display "Nome da Mae: "
           accept ws-mae

           display "Nome do Pai: "
           accept ws-pai

           display "Telefone   : "
           accept ws-telefone

      *>   Movendo os Dados Para o Arquivo
           move ws-aluno     to fd-aluno
           move ws-endereco  to fd-endereco
           move ws-mae       to fd-mae
           move ws-pai       to fd-pai
           move ws-telefone  to fd-telefone

      *>   Reescrever Dados no Arquivo
           rewrite fd-alunos

      *>   Tratamento de Erro - Caso O File Status dê Igual Zero a Informação Será Alterada com Sucesso
           if  ws-fs-arqCadAlunosIndex = 0 then
               display "Aluno de Codigo " ws-cod-aluno " Alterado Com Sucesso!"
           else
      *>       Tratamento de Erro - Caso O File Status dê Igual 23 o Registro Não Existe (Nesse Caso o Codigo)
               if ws-fs-arqCadAlunosIndex = 23 then
                   display "Codigo Informado Invalido!"
               else
                   move 9                                             to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunosIndex                       to ws-msn-erro-cod
                   move "Erro ao Reescrever arq. arqCadAlunosIndex "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           .
       alterar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>                      Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.

      *>   Caso Finalize de Forma Anormal a Mensagem de Erro Aparecerá
      *>   A Mensagem é Composta por um Código, o File Status e Uma Descrição do Erro que Está Ocorrendo
           display erase
           display ws-msn-erro.

           stop run

           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>                          Finalização
      *>------------------------------------------------------------------------
       finaliza section.

      *>   Fecha o Arquivo
           close arqCadAlunosIndex
      *>   Tratamento de Erro - Caso O File Status dê Diferente de Zero Aparecerá a Mensagem de Erro
           if ws-fs-arqCadAlunosIndex <> 0 then
               move 10                                         to ws-msn-erro-ofsset
               move ws-fs-arqCadAlunosIndex                    to ws-msn-erro-cod
               move "Erro ao fechar arq. arqCadAlunosIndex "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           stop run

           .
       finaliza-exit.
           exit.


