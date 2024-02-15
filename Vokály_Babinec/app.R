

library(shiny)
library(tuneR)


ə <- readWave("above.wav") #načtení zvukových souborů
æ <- readWave("bad.wav")
eɪ <- readWave("bayed.wav")
i <- readWave("bead.wav")
e <- readWave("bed.wav")
ɪ <- readWave("bid.wav")
ɜː <- readWave("bird.wav")
oʊ <- readWave("bode.wav")
u <- readWave("booed.wav")
aʊ <- readWave("bough.wav")
oɪ <- readWave("boy.wav")
ʌ <- readWave("bud.wav")
aɪ <- readWave("buy.wav")
ʊ <- readWave("good.wav")
ɑ <- readWave("pod.wav")

ui <- fluidPage(
  
  titlePanel("Poznej vokál z americké angličiny"), #Nadpis
  
  sidebarLayout( #Rozložení s tlačítky a seznamem
    sidebarPanel(
      actionButton("zvuk", "Přehraj vokál"),
      selectInput("vyber", "Zvol správný vokál", c("", "ə", "æ", "eɪ","i","e","ɪ","ɜː","oʊ","u","aʊ","oɪ","ʌ","aɪ","ʊ","ɑ")),
      actionButton("dalsi", "Další"),
      actionButton("kontrola", "Zkontrolovat"),
      br(),
      br(),
      actionButton("restart", "Hrát znovu")
      
    ),
  mainPanel(  #Textová pole
    textOutput("cislootazky"),
    textOutput("skore"),
    br(),
    textOutput("vyhodnoceni"),
    textOutput("spravne"),
    br(),
    br(),
    br(),
    textOutput("navod")
    
    
  )
  )
  
   
  
)



server <- function(input, output) {


    
  seznam <- list(
    ə = "ə" <- readWave("above.wav"), #Seznam všech zvukových souborů
    æ = "æ" <- readWave("bad.wav"),
    eɪ = "eɪ" <- readWave("bayed.wav"),
    i = "i" <- readWave("bead.wav"),
    e = "e" <- readWave("bed.wav"),
    ɪ = "ɪ" <- readWave("bid.wav"),
    ɜː = "ɜː" <- readWave("bird.wav"),
    oʊ = "oʊ" <- readWave("bode.wav"),
    u = "u" <- readWave("booed.wav"),
    aʊ = "aʊ" <- readWave("bough.wav"),
    oɪ = "oɪ" <- readWave("boy.wav"),
    ʌ = "ʌ" <- readWave("bud.wav"),
    aɪ = "aɪ" <- readWave("buy.wav"),
    ʊ = "ʊ" <- readWave("good.wav"),
    ɑ = "ɑ" <- readWave("pod.wav"))

poradi <- reactiveVal(sample(names(seznam))) #Náhodné pořadí bez opakování


#rv <- reactiveValues() #Prázdná množina reaktivnách proměnných
#pocet <- 0 #Reaktivní proměnná, která říká, kolikátý zvuk hraje
rvpocet <- reactiveVal(1) #Reaktivní proměnná, která říká, kolikátý zvuk hraje
rvskore <- reactiveVal(0) #Reaktivní proměnná, která počítá skóre
rvhodnoceni <- reactiveVal(0) #Reaktivní proměnná, díky které program pozná, zda bylo stisknuto tlačítko pro kontrolu

observeEvent(input$zvuk, { #Reaktivní prostředí

  
  play(seznam[[poradi()[rvpocet()]]]) #Přehraj zvukový soubor z vektoru "seznam" na základě náhodného "poradi", které vybírá tolikátý prvek jako je hodnota "rvpocet()"

})

observeEvent(input$dalsi,{ #Při stisku tlačítka Další
  
  if (rvpocet() == 15 & rvhodnoceni() != 0) { #Pokud jsme u poslední otázky a už byla zkontrolována
              
      output$spravne <- renderText("Konec hry, pro novou hru stiskni Hrát znovu.")
      
      if (rvskore() == 15){ #Při skóre 15
      
        output$vyhodnoceni <- renderText("Plný počet, jsi opravdový mistr vokálů")
        
      } else if (rvskore() >= 11 & rvskore() <= 14) {
        
        output$vyhodnoceni <- renderText("Slušný výkon, zkus to znovu a vychytej chyby")
        
      } else if (rvskore() >= 6 & rvskore() <= 10) {
        
        output$vyhodnoceni <- renderText("Pokus dobrý, ale je hodně místa pro zlepšení")
        
      } else if (rvskore() >= 3 & rvskore() <= 5) {
        
        output$vyhodnoceni <- renderText("Máš hodně na čem pracovat, co takhle dát si novou hru?")
        
      } else {
        
        output$vyhodnoceni <- renderText("Budeme dělat, že jsi to celé jenom neproklikal, a dáš si to znova")
        
      }

    } else if (rvhodnoceni() == 0) { #"rvhodnoceni" je proměnná, která se při každé nové otázce resp. zvuku resetuje na 0
    
      output$vyhodnoceni <- renderText("Nejprve odpověz a svou odpověď nech zkontrolovat!") #Pokud je "rvhodnoceni" == 0, pak není možné přejít na další zadání
  
    } else {
  
      output$vyhodnoceni <- renderText("Který vokál ve slově slyšíš?")
    
      rvpocet(rvpocet()+1) #Při stistku tlačítka "Další" přidá k počtu 1
    
      rvhodnoceni(0)
  
      output$spravne <- renderText("")
      
  }           
             })
observeEvent(input$kontrola,{ #Při stisku Zkontroluj
  
  if (input$vyber == "" & rvhodnoceni() == 0) { 
    
    output$vyhodnoceni <- renderText("Vyber správný vokál!") #Při nevybrání možnosti, resp. při vybrání "", vrátí zprávu
 
     } else if (input$vyber == poradi()[[rvpocet()]] & rvhodnoceni() == 0) { #Pokud se výběr uživatele ze seznamu rovná názvu proměnné s přehraným zvukem
     
       rvhodnoceni(rvhodnoceni() + 1) #Kontrola toho, že byla odpověď zkontrolována
       
       rvskore(rvskore() + 1) #Přičtení 1 ke skóre
       
       output$vyhodnoceni <- renderText(sample(c("Správně, jsi jednička", "Velice dobře, jako rozený fonetik", "Přesně tak, v USA se neztratíš", "Super, nechápu jak to děláš"), size = 1))
       #Ukáže jednu ze 4 motivačních zpráv
       
       
     
     } else if (rvhodnoceni() == 0 & input$vyber != poradi()[[rvpocet()]]) { #Špatná odpověď ale ne ""
       
       output$vyhodnoceni <- renderText(sample(c("Netrefil ses, ale nezoufej, žádný učený z nebe nespadl", "Tohle je sice špatně, ale příště to určitě vyjde", "Nene, že by špatná sluchátka?"), size = 1))
       #Ukáže zprávu o tom, že odpověď nebyla správná
       
       output$spravne <- renderText(paste("Správná odpověď je ", poradi()[[rvpocet()]]))
       
       rvhodnoceni(rvhodnoceni() + 1) #Kontrola toho, že byla odpověď zkontrolována
     } else {} 
    
  
  
  
  
 
  
})

observeEvent(input$restart, { #Dialogové okno při kliknutí na restart
  
  showModal(modalDialog(
    h1("Opravdu chcete restartovat hru?"),
    actionButton("ano", "Ano"), #Tlačítko pro možnost ano
    modalButton("Ne"), #Tlačítko pro možnost ne - není potřeba observeEvent, protože tlačítko automaticky zavírá okno
    footer = NULL #Nastavení aby v okně byla jen 2 tlačítka
  ))
  
})
observeEvent(input$ano, { #Při kliknutí na možnost Ano
  
  removeModal() #Zavírá dialogové okno
  rvskore(0)
  rvpocet(1)
  rvhodnoceni(0) #Resetuje proměnné
  
  #poradi <- sample(names(seznam)) #Náhodné pořadí bez opakování
  poradi(sample(names(seznam)))
  
  output$vyhodnoceni <- renderText("Nová hra!")
  output$spravne <- renderText("")
  
})

output$skore <- renderText(paste("Skóre: ", rvskore(), "/15")) #Zobrazení skóre

output$cislootazky <- renderText(paste("Vokál číslo ", rvpocet())) #Zobrazení čísla otázky 

output$navod <- renderText("V této hře je tvým úkolem správně určit vokál, který se objevuje v přehraném slově.
                           Pokud má slovo více slabik, počítá se první slabika.
                           Pro začátek hry stiskni tlačítko Přehraj vokál (spustí se přehrávač, který je potřeba manuálně vypínat), vyber odpovídající vokál z nabídky a stiskni Zkontrolovat.
                           Pro další slovo stiskni Další.
                           Pro novou hru stiskni Hrát znovu")

}

# Run the application 
shinyApp(ui = ui, server = server)

