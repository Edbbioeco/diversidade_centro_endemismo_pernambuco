# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_status() |>
  as.data.frame()

# Adicionando os arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para comandos de git")

# Pushando ----

gert::git_push(remote = "cep")

# Pullando ----

gert::git_pull(remote = "cep")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD~1")
