# Instalando as bibliotecas
install.packages("rpart")
install.packages("rpart.plot")
# Carregando bibliotecas
library(rpart)
library(rpart.plot)
# Lendo o arquivo
recipes <- read.csv("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DS0103EN-SkillsNetwork/labs/Module%202/recipes.csv", header = TRUE, sep=",")
# Imprimindo as primeiras linhas
head(recipes)
# Número de linhas e colunas
nrow(recipes)
ncol(recipes)
# Para verificar se existe arroz (Rice) no DataFrame
grep("rice", names(recipes), value=TRUE)
# Tabela de frequência do DF por país
base::table(recipes$country)
# Pela tabela acima podemos notar alguns problemas: A coluna cozinha está como país, nem todos os nomes das cozinhas começam com letra maiuscula, algumas estão duplicadas e algumas tem muito poucos ingredientes.
# Renomeando a coluna (cuisine = cozinha)
colnames(recipes)[1] = "cuisine"
# Transformar todos os nomes em minusculo
recipes$cuisine <- tolower(as.character(recipes$cuisine))
# Renomeando os duplicados
recipes$cuisine[recipes$cuisine == "austria"] <- "austrian"
recipes$cuisine[recipes$cuisine == "belgium"] <- "belgian"
recipes$cuisine[recipes$cuisine == "china"] <- "chinese"
recipes$cuisine[recipes$cuisine == "canada"] <- "canadian"
recipes$cuisine[recipes$cuisine == "netherlands"] <- "dutch"
recipes$cuisine[recipes$cuisine == "france"] <- "french"
recipes$cuisine[recipes$cuisine == "germany"] <- "german"
recipes$cuisine[recipes$cuisine == "india"] <- "indian"
recipes$cuisine[recipes$cuisine == "indonesia"] <- "indonesian"
recipes$cuisine[recipes$cuisine == "iran"] <- "iranian"
recipes$cuisine[recipes$cuisine == "israel"] <- "jewish"
recipes$cuisine[recipes$cuisine == "italy"] <- "italian"
recipes$cuisine[recipes$cuisine == "japan"] <- "japanese"
recipes$cuisine[recipes$cuisine == "korea"] <- "korean"
recipes$cuisine[recipes$cuisine == "lebanon"] <- "lebanese"
recipes$cuisine[recipes$cuisine == "malaysia"] <- "malaysian"
recipes$cuisine[recipes$cuisine == "mexico"] <- "mexican"
recipes$cuisine[recipes$cuisine == "pakistan"] <- "pakistani"
recipes$cuisine[recipes$cuisine == "philippines"] <- "philippine"
recipes$cuisine[recipes$cuisine == "scandinavia"] <- "scandinavian"
recipes$cuisine[recipes$cuisine == "spain"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "portugal"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "switzerland"] <- "swiss"
recipes$cuisine[recipes$cuisine == "thailand"] <- "thai"
recipes$cuisine[recipes$cuisine == "turkey"] <- "turkish"
recipes$cuisine[recipes$cuisine == "irish"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "uk-and-ireland"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "vietnam"] <- "vietnamese"
# Removendo os que tem menos que 50 ingredientes
# Classificando a tabela em ordem decrescente
t <- sort(base::table(recipes$cuisine), decreasing = T)
# Selecionando as que tem mais que 50
filter_list <- names(t[t >= 50])
# Aplicando o filtro no DF
recipes <- recipes[recipes$cuisine %in% filter_list,]
# Convertendo todas as colunas em factors
recipes[,names(recipes)] <- lapply(recipes[,names(recipes)], as.factor)
# Checando a estrutura dos dados
str(recipes)
# Vamos construir a arvore de decisão usando os dados pertencentes as cozinhas asiaticas e indianas
bamboo_tree <- rpart(formula=cuisine ~ ., data=recipes[recipes$cuisine %in% c("korean", 
                                                                              "japanese", 
                                                                              "chinese", 
                                                                              "thai",
                                                                              "indian"),], method ="class")
# Exibindo a arvore de decisão
rpart.plot(bamboo_tree, type=3, extra=2, under=TRUE, cex=0.75, varlen=0, faclen=0)
# Criando um novo DF usando apenas os dados pertencentes as cozinhas asiaticas e indiana
bamboo <- recipes[recipes$cuisine %in% c("korean", "japanese", "chinese", "thai", "indian"),]
# Observando quantos ingredientes existem para cada cozinha
base::table(as.factor(as.character(bamboo$cuisine)))
# Vamos remover 30 ingredientes de cada cozinha para usar como teste
sample_n <- 30
set.seed(4)
korean <- bamboo[base::sample(which(bamboo$cuisine == "korean") , sample_n), ]
japanese <- bamboo[base::sample(which(bamboo$cuisine == "japanese") , sample_n), ]
chinese <- bamboo[base::sample(which(bamboo$cuisine == "chinese") , sample_n), ]
thai <- bamboo[base::sample(which(bamboo$cuisine == "thai") , sample_n), ]
indian <- bamboo[base::sample(which(bamboo$cuisine == "indian") , sample_n), ]
bamboo_test <- rbind(korean, japanese, chinese, thai, indian)
# Checando o DF teste
base::table(as.factor(as.character(bamboo_test$cuisine)))
# Agora vamos criar a base de dados para treinar o modelo, removendo os dados do teste do principal DF
bamboo_train <- bamboo[!(rownames(bamboo) %in% rownames(bamboo_test)),]
# Checando o novo DF
base::table(as.factor(as.character(bamboo_train$cuisine)))
# Construindo a arvore de decisão com a base de dados para treino
bamboo_train_tree <- rpart(formula=cuisine ~ ., data=bamboo_train, method="class")
# Exibindo a arvore de decisão
rpart.plot(bamboo_train_tree, type=3, extra=2, under=TRUE, cex=0.75, varlen=0, faclen=0)
# Agora vamos testar o modelo com os dados para teste
bamboo_pred_cuisines <- predict(bamboo_train_tree, subset(bamboo_test, select=-c(cuisine)), type="class")
# Criando uma matriz de confusão para saber o quão bem o modelo performou
bamboo_confusion_matrix <- base::table(
  paste(as.character(bamboo_test$cuisine),"_true", sep=""),
  paste(as.character(bamboo_pred_cuisines),"_pred", sep="")
)

round(prop.table(bamboo_confusion_matrix, 1)*100, 1)
# Cada linha representa a verdadeira cozinha e as colunas representam as predições
# Observações: 60% da cozinha chinesa foi corretamente classificada, 36,7% foi classificada erroneamente como coreana e 3,3% como japonesa;
# 90% da cozinha indiana foi corretamente classificada e 10% foi classificado erroneamente como coreana; e assim sucessivamente.