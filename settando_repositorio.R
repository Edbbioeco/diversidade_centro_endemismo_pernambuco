# Pacotes ----

library(usethis)

# Iniciando a sessão ----

usethis::use_git()

# Settando usuário e e-mail ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Checando o repositório ----

usethis::proj_get()

# Settando o repositório ----

usethis::use_git_remote(name = "cep",
                        url = "https://github.com/Edbbioeco/diversidade_centro_endemismo_pernambuco.git",
                        overwrite = TRUE)
