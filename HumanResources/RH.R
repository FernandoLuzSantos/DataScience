# Projeto: Ferramenta de apoio para tomada de decisão em RH e visão geral da empresa.

# Este projeto é uma análise exploratória de dados e seleção de atributos como ferramenta de apoio ao RH para contratação e métricas de turnover.
# O dataset é público e refere-se à empresa IBM.

# Configurando o diretório de trabalho
setwd('C:/Users/fluzd/OneDrive/Documentos/Programacao/PortfolioGitHub/EDA')
getwd()

install.packages("randomForest")
# Pacotes necessários para o desenolvimento deste projeto
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(reshape2)
library(ggplot2)
library(randomForest)

# Carregando o dataset
dados_rh <- fread('dados/dataset.csv')
dim(dados_rh)
View(dados_rh)
str(dados_rh)
summary(dados_rh)

##### Limpeza e Transformação ##### 

# Transformando variáveis categóricas para o tipo fator
View(dados_rh)
dados_rh$Attrition                <- as.factor(dados_rh$Attrition)
dados_rh$BusinessTravel           <- as.factor(dados_rh$BusinessTravel)
dados_rh$Department               <- as.factor(dados_rh$Department)
dados_rh$Education                <- as.factor(dados_rh$Education)
dados_rh$EducationField           <- as.factor(dados_rh$EducationField)
dados_rh$'Employee Source'        <- as.factor(dados_rh$'Employee Source')
dados_rh$EnvironmentSatisfaction  <- as.factor(dados_rh$EnvironmentSatisfaction)
dados_rh$Gender                   <- as.factor(dados_rh$Gender)
dados_rh$JobInvolvement           <- as.factor(dados_rh$JobInvolvement)
dados_rh$JobLevel                 <- as.factor(dados_rh$JobLevel)
dados_rh$JobRole                  <- as.factor(dados_rh$JobRole)
dados_rh$JobSatisfaction          <- as.factor(dados_rh$JobSatisfaction)
dados_rh$MaritalStatus            <- as.factor(dados_rh$MaritalStatus)
dados_rh$OverTime                 <- as.factor(dados_rh$OverTime)
dados_rh$PerformanceRating        <- as.factor(dados_rh$PerformanceRating)
dados_rh$RelationshipSatisfaction <- as.factor(dados_rh$RelationshipSatisfaction)
dados_rh$StockOptionLevel         <- as.factor(dados_rh$StockOptionLevel)
dados_rh$WorkLifeBalance          <- as.factor(dados_rh$WorkLifeBalance)
str(dados_rh)

# Transformando variáveis numéricas para o tipo inteiro
View(dados_rh)
dados_rh$DistanceFromHome  <- as.integer(dados_rh$DistanceFromHome)
dados_rh$MonthlyIncome     <- as.integer(dados_rh$MonthlyIncome)
dados_rh$PercentSalaryHike <- as.integer(dados_rh$PercentSalaryHike)

# Drop dos níveis de fatores com 0 count
dados <- droplevels(dados_rh)
str(dados_rh)
summary(dados_rh)
View(dados_rh)

##### Engenharia de Atributos ##### 

# Análise perfil de experiência do funcionário. Criação de coluna com anos de experiência prévia.
dados_rh$PriorYearsOfExperience <- dados_rh$TotalWorkingYears - dados_rh$YearsAtCompany
View(dados_rh)

# Estabilidade média (job tenure): numero médio de anos que o funcionario ficou em cada empresa trabalhada anteriormente
dados_rh$EstabilidadeMédia <- dados_rh$PriorYearsOfExperience / dados_rh$NumCompaniesWorked
View(dados_rh)

# Caso sejam gerados valores infinitos devido a divisão por zero, é feita a substituição por zero.
summary(dados_rh$EstabilidadeMédia)
dados_rh$EstabilidadeMédia[!is.finite(dados_rh$EstabilidadeMédia)] <- 0
summary(dados_rh$EstabilidadeMédia)
View(dados_rh)

# Divisão do dataset original em dois datasets com base na coluna Termination, que indica se o funcionário foi desligado da empresa, pediu demissão voluntariamente ou é funcionário atualmente.

# dados_rh_1 refere-se àqueles funcionários atuais ou que pediram demissão voluntária.
dados_rh_1 <- dados_rh[dados_rh$Attrition != 'Termination']
dados_rh_1 <- droplevels(dados_rh_1)
dim(dados_rh_1)
summary(dados_rh_1)

# dados_rh_2 refere-se àqueles funcionários atuais ou que foram desligados.

dados_rh_2 <- dados_rh[dados_rh$Attrition != 'Voluntary Resignation']
dados_rh_2 <-droplevels(dados_rh_2)
dim(dados_rh_2)  
summary(dados_rh_2)

##### Análise Exploratória ##### 

# Plots de análise univariada
ggplot(dados_rh) + geom_bar(aes(x = Gender)) #maior presença de homens na empresa
ggplot(dados_rh) + geom_density(aes(x = Age)) #maior parte dos funcionarios tem entre 30 e 35 anos
ggplot(dados_rh) + geom_bar(aes(x = Attrition)) # maior parte são funcionarios atualmente, minoria foi demitida
ggplot(dados_rh) + geom_bar(aes(x = Department)) # maior concentração de pessoas no departamento de pesquisa e desenolvimento
ggplot(dados_rh) + geom_bar(aes(x = JobRole)) # tecnico de laboratorio, pesquisador e executivo de vendas são as funções com maior numero de funcionarios
ggplot(dados_rh) + geom_bar(aes(x = Department)) + facet_grid(~BusinessTravel) #baixo numero de funcionarios que viajam a  trabalho
ggplot(dados_rh) + geom_bar(aes(x = Gender)) + facet_grid(~EnvironmentSatisfaction) # alto indice de satisfação no ambiente de trabalho
ggplot(dados_rh) + geom_density(aes(Age)) + facet_grid(~EducationField)
str(dados_rh$Education) # Nivel 5 corresponde a doutorado, nivel 1 corresponde a nivel medio

# Multiplot Grid
p.TotalWorkingYears       <- ggplot(dados_rh) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(dados_rh) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(dados_rh) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(dados_rh) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(dados_rh) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(dados_rh) + geom_density(aes(PriorYearsOfExperience))


# Organiza no grid
grid.arrange(p.TotalWorkingYears, 
             p.YearsAtCompany, 
             p.YearsSinceLastPromotion, 
             p.YearsWithCurrManager, 
             p.YearsInCurrentRole, 
             p.PriorYearsOfExperience, 
             nrow = 2, 
             ncol = 3)

# Tempo de experiência anterior
length(which(dados_rh$PriorYearsOfExperience < 1)) / length(dados_rh$PriorYearsOfExperience)  
length(which(dados_rh$PriorYearsOfExperience < 3)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 5)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 7)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 10)) / length(dados_rh$PriorYearsOfExperience)  

# 70% dos funcionários têm menos de 5 anos de experiência de trabalho antes de entrar na IBM
# Possíveis problemas: conjuntos de habilidades subdesenvolvidos, base de jovens funcionários, 
# mentalidade imatura

# Idade
length(which(dados_rh$Age < 30)) / length(dados_rh$Age)

# Apenas 22% dos funcionários têm menos de 30 anos, a base de funcionários não é exatamente 
# tão jovem como o esperado. Entende-se que os funcionários começaram a trabalhar relativamente tarde.

# # Educação
summary(dados_rh$Education)
length(which(dados_rh$Education == 1)) / length(dados_rh$Education) # 11,5% possuem nivel medio
length(which(dados_rh$Education == 2)) / length(dados_rh$Education) # 19% possuem nivel tecnico
length(which(dados_rh$Education == 3)) / length(dados_rh$Education) # 38,7% são graduados
length(which(dados_rh$Education == 4)) / length(dados_rh$Education) # 27% realizaram mestrado
length(which(dados_rh$Education == 5)) / length(dados_rh$Education) # 3,2% possuem doutorado

# Boxplot satisfação no trabalho x media salarial(1- insatisfeito, 4 muito satisfeito) 
ggplot(data = subset(dados_rh, !is.na(JobSatisfaction)), aes(JobSatisfaction, MonthlyIncome)) + 
  geom_boxplot()
# Não há sinais de que um salário mais alto leva a uma maior satisfação no trabalho

# Correlação
cor(dados_rh$TotalWorkingYears, dados_rh$YearsAtCompany,          use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsInCurrentRole,      use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsSinceLastPromotion, use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsWithCurrManager,    use = "complete.obs")
cor(dados_rh$TotalWorkingYears, dados_rh$MonthlyIncome,           use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$MonthlyIncome,           use = "complete.obs")  

# Scatterplots
ggplot(dados_rh) + geom_point(aes(TotalWorkingYears, MonthlyIncome)) # correlação elevada
ggplot(dados_rh) + geom_point(aes(YearsAtCompany, MonthlyIncome)) # correlação elevada

# Vamos investigar a relação do equilíbrio entre vida pessoal e profissional e renda mensal
ggplot(data = subset(dados_rh, !is.na(WorkLifeBalance)), aes(WorkLifeBalance, MonthlyIncome)) + 
  geom_boxplot()
# Os funcionários que avaliaram o equilíbrio entre vida profissional e pessoal igual a 1 também têm renda média mensal 
# significativamente mais baixa.


# Verificando a diferença salarial entre homens e mulheres.
ggplot(data = subset(dados_rh, !is.na(Gender)), aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(x = "Gender", y = "Monthly Income", title = "Salário Mensal Entre Gêneros") +
  coord_flip()
# Não há sinais de discriminação de gênero; na verdade, as mulheres ganham um pouco mais, em média, 
# desconsiderando todos os outros fatores.

# Função
ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, MonthlyIncome)) +
  ggtitle("Salário Mensal Por Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, AgeStartedWorking)) +
  ggtitle("Relação entre idade que começou a trabalhar e função que ocupa")
#Menores médias de idade para gerente e diretor de pesquisa. Maiores médias de idade para representante de vendas e técnico de laboratorio. 

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, Age)) +
  ggtitle("Idade Por Função")
# indica maior idade média para funções de gerencia e direção.

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, YearsAtCompany)) +
  ggtitle("Tempo de Empresa (em anos)")
# indica os gerentes com maior tempo medio de empresa

# Plots de análise multivariada para variáveis normalmente usadas durante o processo de contratação

ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~Department)

ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole)

ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = EducationField , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

# Plots de análise multivariada para variáveis normalmente usadas após o processo de contratação
ggplot(dados_rh_1) + geom_bar(aes(x = Age, fill = Attrition), position = 'fill') # mais pedidos de demissao voluntaria para funcionarios abaixo de 30 anos.
ggplot(dados_rh_1) + geom_bar(aes(x = Department, fill = Attrition), position = 'fill') # menor indice de pedidos de demissão voluntaria no departamento de pesquisa e desenolvimento
ggplot(dados_rh_1) + geom_bar(aes(x = DistanceFromHome, fill = Attrition), position = 'fill') # menor retenção de funcionários que moram longe da empresa
ggplot(dados_rh_1) + geom_bar(aes(x = `Employee Source`, fill = Attrition), position = 'fill') # contratação por indeed e linkedin apresentaram maior retenção na empresa
ggplot(dados_rh_1) + geom_bar(aes(x = JobRole, fill = Attrition), position = 'fill') # representante de vendas, tecnico de laboratorio e recursos humanos são áreas com maior indice de pedidos de demissao
ggplot(dados_rh_1) + geom_bar(aes(x = MaritalStatus, fill = Attrition), position = 'fill') # funcionarios solteiros historicamente tem maior proporção dos pedidos de demissao voluntaria
ggplot(dados_rh_1) + geom_density(aes(x = EstabilidadeMédia, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = Education, fill = Attrition), position = 'fill') # maior retenção de funcionários na empresa conforme maior o nivel de estudo
ggplot(dados_rh_1) + geom_bar(aes(x = Gender, fill = Attrition), position = 'fill') # genero do funcionario nao parece ter relação com attrition

# Plots de análise multivariada entre algumas variáveis e o status do funcionário
ggplot(dados_rh_1) + geom_boxplot(aes(Attrition, MonthlyIncome)) # média salarial de funcionarios que pediram demissão voluntaria era menor do que os funcionarios atuais.
ggplot(dados_rh_1) + geom_boxplot(aes(Attrition, PercentSalaryHike)) # concessão de aumento salarial não parece ter relação com pedidos de demissão voluntaria
ggplot(dados_rh_1) + geom_bar(aes(TrainingTimesLastYear, fill = Attrition), position = 'fill') # funcionários que recebem mais treinamentos tendem a permanecer na empresa. Indica que treinamentos são uteis par retenção de funcionarios.
ggplot(dados_rh_1) + geom_bar(aes(BusinessTravel, fill = Attrition), position = 'fill') # maior indice de pedidos de demissão em funcionarios que viajam frequentemente a trabalho.
ggplot(dados_rh_1) + geom_bar(aes(OverTime, fill = Attrition), position = 'fill') # maior indice de pedidos de demissão em funcionarios que fazem hora extra.
ggplot(dados_rh_1) + geom_bar(aes(StockOptionLevel, fill = Attrition), position = 'fill') # maior indice de pedidos de demissão em funcionarios que nao possuem participação acionaria.
ggplot(dados_rh_1) + geom_bar(aes(EnvironmentSatisfaction, fill = Attrition), position = 'fill') # a satisfação com o ambiente de trabalho tem correlação negativa com o indice de pedidos de demissao voluntaria 
ggplot(dados_rh_1) + geom_bar(aes(JobSatisfaction, fill = Attrition), position = 'fill') # a satisfação com o trabalho possui relação direta com a retenção de funcionarios na empresa
ggplot(dados_rh_1) + geom_bar(aes(JobInvolvement, fill = Attrition), position = 'fill') # o engajamento no trabalho possui relação direta com a retenção de funcionarios na empresa.
ggplot(dados_rh_1) + geom_bar(aes(RelationshipSatisfaction, fill = Attrition), position = 'fill') # não parece haver correlação entre pedidos de demissao voluntaria e satisfação nos relacionamentos conjugais
ggplot(dados_rh_1) + geom_bar(aes(WorkLifeBalance, fill = Attrition), position = 'fill') # maior indice de pedidos de demissão voluntaria em funcionários com pior equilibrio entre vida pessoal e trabalho.

# Três categorizações para Attrition
str(dados_rh$Attrition)
# overall_retention_rate é o inverso do turnover, indica o indice de retenção de funcionarios na empresa em um certo tempo.
Overall_retention_rate <- (length(which(dados_rh$Attrition == "Current employee")) - length(which(dados_rh$Attrition != "Current employee")))/length(which(dados_rh$Attrition == "Current employee"))
Overall_retention_rate

# Turnover rate:
Overall_turnover_rate <- 1/Overall_retention_rate -1 
Overall_turnover_rate
#Voluntary turnover
Voluntary_turnover <- length(which(dados_rh$Attrition == "Voluntary Resignation"))/length(which(dados_rh$Attrition == "Current employee"))
Voluntary_turnover
#Involuntary turnover rate
Involuntary_turnover <- length(which(dados_rh$Attrition == "Termination"))/length(which(dados_rh$Attrition == "Current employee"))
Involuntary_turnover
#Average length of employment (years average length of employment)
LengthOfEmployment <- sum(dados_rh$YearsAtCompany) / nrow(dados_rh)
LengthOfEmployment
#New employee satisfaction rate - considerados funcionarios novos aqueles com 1 ano de empresa
ggplot(dados_rh[dados_rh$YearsAtCompany < 2]) + geom_bar(aes(JobSatisfaction, fill = Attrition), position = 'fill') # maiores pedidos de demissão em funcionarios menos satisfeitos com o trabalho
ggplot(dados_rh[dados_rh$YearsAtCompany < 2]) + geom_bar(aes(JobInvolvement, fill = Attrition), position = 'fill') # menor engajamento no trabalho em novos funcionarios tem sido correlacionado com 
#New hire retention
New_hire_retention_rate <- length(which(dados_rh$YearsAtCompany < 2 & dados_rh$Attrition != 'Current Employee')) / length(which(dados_rh$Attrition != 'Current Employee'))
New_hire_retention_rate
# turnover por departamento - maior numero de demissões no dpto de pesquisa e desenvolvimento. Não houveram demissoes no setor de recursos humanos.
Not_emplyees <- dados_rh[dados_rh$Attrition != 'Current employee']
ggplot(Not_emplyees) + geom_bar(aes(Department, fill = Attrition), position = 'fill')
# É sabido que nenhum funcionário ficará para sempre na empresa, não importa o quão boa ela seja. Os calculos anteriores foram feitos baseado no total de funcionarios presentes no dataset original, sendo que nao foi informado o período exato de coleta dos dados.

#Preparação final das variáveis categoricas para dados de entrada no modelo de aprendizagem de maquina
# utilização de variavel dummy
dados_rh <- dados_rh %>%
  mutate(target = ifelse(Attrition == 'Current employee', 1, 0))

dados_rh <- subset(dados_rh, select = -Attrition)

dados_rh <- dados_rh %>%
  mutate(OverTime = ifelse(OverTime == 'Yes', 1, 0))

dados_rh <- dados_rh %>%
  mutate(BusinessTravel = 
           case_when(BusinessTravel == 'Non-Travel' ~ '0',
                     BusinessTravel == 'Travel_Rarely' ~ '1',
                     BusinessTravel == 'Travel_Frequently' ~ '2'))
str(dados$Department)
dados_rh <- dados_rh %>%
  mutate(Department = 
           case_when(Department == 'Sales' ~ '0',
                     Department == 'Human Resources' ~ '1',
                     Department == 'Research & Development' ~ '2'))
str(dados$EducationField)
unique(dados_rh$EducationField)
dados_rh <- dados_rh %>%
  mutate(EducationField = 
           case_when(EducationField == 'Human Resources' ~ '0',
                     EducationField == 'Life Sciences' ~ '1',
                     EducationField == 'Marketing' ~ '2',
                     EducationField == 'Technical Degree' ~ '3',
                     EducationField == 'Other' ~ '4',
                     EducationField == 'Medical' ~ '5'))
unique(dados_rh$Gender)
dados_rh <- dados_rh %>%
  mutate(Gender = ifelse(Gender == 'Female', 1, 0))
unique(dados_rh$MaritalStatus)
dados_rh <- dados_rh %>%
  mutate(MaritalStatus = 
           case_when(MaritalStatus == 'Divorced' ~ '0',
                     MaritalStatus == 'Married' ~ '1',
                     MaritalStatus == 'Single' ~ '2'))
unique(dados_rh$JobRole)
dados_rh <- dados_rh %>%
  mutate(JobRole = 
           case_when(JobRole == 'Sales Executive' ~ '0',
                     JobRole == 'Manager' ~ '1',
                     JobRole == 'Human Resources' ~ '2',
                     JobRole == 'Research Scientist' ~ '3',
                     JobRole == 'Manufacturing Director' ~ '4',
                     JobRole == 'Laboratory Technician' ~ '5',
                     JobRole == 'Healthcare Representative' ~ '6',
                     JobRole == 'Sales Representative' ~ '7',
                     JobRole == 'Research Director' ~ '8'))
unique(dados_rh$`Employee Source`)
dados_rh <- dados_rh %>%
  mutate(`Employee Source` = 
           case_when(`Employee Source` == 'Referral' ~ '0',
                     `Employee Source` == 'Company Website' ~ '1',
                     `Employee Source` == 'Indeed' ~ '2',
                     `Employee Source` == 'Seek' ~ '3',
                     `Employee Source` == 'Adzuna' ~ '4',
                     `Employee Source` == 'Recruit.net' ~ '5',
                     `Employee Source` == 'GlassDoor' ~ '6',
                     `Employee Source` == 'Jora' ~ '7',
                     `Employee Source` == 'LinkedIn' ~ '8'))
View(dados_rh)
##### Modelagem Preditiva #####

#Feature selection

#Nesta etapa, será realizado um estudo dos principais atributos que fizeram os profissionais sairem da empresa, sem diferenciar se o desligamento foi voluntário ou forçado.
# A importancia do feature selection é a remoção de variaveis com informações redundantes no dataset:
# calculo da matriz de correlação entre todos os atributos
dados_rh <- data.frame(lapply(dados_rh, as.numeric))
dados_rh$target <- as.factor(dados_rh$target)

#Resultado grafico da analise de feature selection: Matriz de correlação
correlationMatrix <- round(cor(dados_rh[,1:31]),3)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# eliminação das variaveis altamente correlacionadas. Criterio: corr > 0.75. New_df gerado e utilizado para proximos passos
column_indexes <- highlyCorrelated
new_df <- dados_rh[,-column_indexes]
dim(new_df)
View(new_df)

#a eleição das variaveis com maior importancia:
#definindo sistema de controle
control <- trainControl(method='repeatedcv', number=10, repeats=3)
# realizando o treinamento utilizando o modelo LVQ (learning vector quantization)
model <- train(target~., data=new_df, method='lvq', preProcess='scale', trControl=control)
# estimando a importancia da variavel
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

# numero de variáveis x acurácia do modelo a partir de random forest
#definição do modelo de controle utilizando random forest selection
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# rodando modelo RFE (recursive feature elimination)
results <- rfe(new_df[,1:26], new_df$target, sizes=c(1:8), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))
#A partir desta selação de atributos, será desenvolvido um modelo de apoio à tomada de decisão do RH para recrutar melhor visando evitar turnover na empresa.

# Vamos dividir os dados em treino e teste

index_treino <- sample.split(Y = new_df$Attrition, SplitRatio = 0.7)

new_df_treino <- subset(new_df, train = T)
new_df_teste <- subset(new_df, train = F)

# Primeira versão do modelo com variaveis selecionadas anteriormente

modelo_v1 <- glm.fit(formula = Attrition ~ ,
                 family = binomial,
                 dados = new_df_treino)
summary(modelo_v1)
vif(modelo_v1)

# Previsões
threshold <- 0.5
previsoes <- predict(modelo_v1, type = 'response', newdata = new_df_teste)
previsoes_finais <- ifelse(previsoes > threshold, 0, 1)
table(new_df_teste$target, previsoes_finais)

# Segunda versão do modelo com outro algoritmo
?rpart
modelo_v2 <- rpart(Attrition ~  
                   method = "class", 
                   control = rpart.control(minsplit = 500, cp = 0),
                   data = dados_rh_1_treino)

summary(modelo_v2)
rpart.plot(modelo_v2)