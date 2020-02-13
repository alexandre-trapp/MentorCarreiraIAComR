library(shiny)
library(sets)

sets_options("universe", seq(1,100,1))

#criacao de variaveis
variaveis <- set(
    Gosta_exatas = fuzzy_partition(varnames = c( gemin = 5, gemen = 15, gemed = 50, gemaior=75, gemax=90), sd = 10),
    Rel_interpess = fuzzy_partition(varnames = c( rimin = 30, rimen = 35, rimed = 55, rimaior=75, rimax=85), sd = 10),
    Escre_codigo = fuzzy_partition(varnames = c( ecmin = 10, ecmen = 25, ecmed = 50, ecmaior=75, ecmax=95),  sd = 10),
    Perfil_lider = fuzzy_partition(varnames = c( plmin = 30, plmen = 50, plmed = 70, plmaior=90, plmax=95), sd=10),
    Gosta_Estudar = fuzzy_partition(varnames = c( gsmin = 20, gsmen = 40, gsmed = 60, gsmaior=80, gsmax=90), sd=10),
    Habilidade_comunicao = fuzzy_partition(varnames = c( hcmin = 40, hcmen = 50, hcmed = 60, hcmaior=70, hcmax=75), sd=10),
    Classificacao = fuzzy_partition(varnames = c( baixa = 10, media = 50, alta = 75, altissima=95), sd=10)
)

#definicao das regras
regras <-
  set(
    fuzzy_rule( Gosta_exatas %is%	gemax	&& Rel_interpess %is%	rimin	&& Escre_codigo %is%	ecmax	&& Perfil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmin	, Classificacao %is%	altissima	),
    fuzzy_rule( Gosta_exatas %is%	gemax	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Perfil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmen	, Classificacao %is%	altissima	),
    fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Perfil_lider %is%	plmin	&& Gosta_Estudar %is%	gsmax	&& Habilidade_comunicao %is%	hcmed	, Classificacao %is%	altissima	),
    fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	rimen	&& Escre_codigo %is%	ecmaior	&& Perfil_lider %is%	plmen	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmed	, Classificacao %is%	alta	),
    fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	 rimed	&& Escre_codigo %is%	ecmed	&& Perfil_lider %is%	plmed	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	alta	),
    fuzzy_rule( Gosta_exatas %is%	gemaior	&& Rel_interpess %is%	 rimed	&& Escre_codigo %is%	ecmed	&& Perfil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmaior	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	alta	),
    fuzzy_rule( Gosta_exatas %is%	gemed	&& Rel_interpess %is%	rimaior	&& Escre_codigo %is%	ecmen	&& Perfil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmaior	, Classificacao %is%	media	),
    fuzzy_rule( Gosta_exatas %is%	gemed	&& Rel_interpess %is%	rimaior	&& Escre_codigo %is%	ecmen	&& Perfil_lider %is%	plmaior	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	media	),
    fuzzy_rule( Gosta_exatas %is%	gemen	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Perfil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmed	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	media	),
    fuzzy_rule( Gosta_exatas %is%	gemen	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Perfil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmen	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	),
    fuzzy_rule( Gosta_exatas %is%	gemin	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Perfil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmen	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	),
    fuzzy_rule( Gosta_exatas %is%	gemin	&& Rel_interpess %is%	rimax	&& Escre_codigo %is%	ecmin	&& Perfil_lider %is%	plmax	&& Gosta_Estudar %is%	gsmin	&& Habilidade_comunicao %is%	hcmax	, Classificacao %is%	baixa	)
   )

sistema = fuzzy_system(variaveis, regras)

# Define UI for application that draws a histogram
ui <- fluidPage(

    
    titlePanel("Aderência de perfil para carreira de cientista de dados"),
    helpText("Defina os parâmetros nos sliders e clique em processar"),
    
    fluidRow(
        
        column(4, sliderInput("sexatas", "Gosto por exatas", min=5, max=90, step=10, value=40)),
        column(4, sliderInput("sinter", "Relacionamento interpessoal", min=30, max=85, step=5, value=50)),
        column(4, sliderInput("scodigo", "Gosto por escrver código", min=10, max=95, step=5, value=40)),
    ),
    
    fluidRow(
        
        column(4, sliderInput("slider", "Perfil de liderança", min=30, max=95, step=5, value=50)),
        column(4, sliderInput("sestudar", "Gosta de estudar", min=20, max=90, step=10, value=40)),
        column(4, sliderInput("scomunica", "Habilidade de comunicação", min=40, max=75, step=5, value=50)),
    ),
    
    fluidRow(
        
        column(6, h1("Sistema:"), plotOutput("GrafSistema")),
        
            column(6, actionButton("Processar", "Processar"), 
            helpText("A linha vermelha mostra a sua aderência a profissão de cientista de dados"),
            plotOutput("GrafResultado")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$GrafSistema = renderPlot({ plot(sistema) })
    observeEvent(input$Processar, {
        
        inferencia = fuzzy_inference(sistema, list(Gosta_exatas = input$sexatas, Rel_interpess = input$sinter, Escre_codigo = input$scodigo, Perfil_lider = input$slider, Gosta_Estudar = input$sestudar,
                                                   Habilidade_comunicao = input$scomunica))
        
        output$GrafResultado = renderPlot({
            
            plot(sistema$variables$Classificacao)
            lines(inferencia, col="red", lwd=4)
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
