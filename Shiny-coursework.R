library(shiny)
library(tidyverse)
library(gapminder)
library(ggthemes)
library(kableExtra)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Life Expectancy Over the Continents"),

    sidebarLayout(
        
        sidebarPanel(
        # check out to filter continent
        checkboxGroupInput(inputId="continents", 
                           label="Continents",
                           choices=c("Africa","Americas","Asia","Europe","Oceania"),
                           selected=c("Africa","Americas","Asia","Europe","Oceania")),
       
        sliderInput(inputId="years",
                    label="Year Range",
                    min=1952,
                    max=2007,
                    value=c(1952,2007),
                    dragRang=T),
        
        tableOutput("summary")
        ),
    
        # plot of life expectancy, group/color by countries, facet by continent
        mainPanel(
           plotOutput("lifeExpPlot"),
           DT::dataTableOutput(outputId="datatable")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$lifeExpPlot <- renderPlot({
        gapminder %>% 
            filter(year>=input$years[1] & year<=input$years[2]) %>% 
            filter(continent %in% input$continents) %>% 
        ggplot(aes(x=year,y=lifeExp,group=country,color=country))+
            geom_line(lwd =1,show.legend=FALSE)+
            facet_wrap(vars(continent),ncol=5)+
            scale_color_manual(values = country_colors)+
            theme_wsj(color='gray')+
            theme(strip.text=element_text(size=rel(1.2),face="bold"),
                  strip.background=element_rect(fill="grey90",color=NA))
        
    })
    
    output$summary <-function(){
        years<-input$years
        from_year<-min(unique(gapminder$year)[unique(gapminder$year)>=years[1]])
        to_year<-max(unique(gapminder$year)[unique(gapminder$year)<=years[2]])
        
        from<-gapminder %>% 
            filter(year==from_year) %>% 
            filter(continent %in% input$continents) %>% 
            group_by(continent) %>% 
            summarize(life0=mean(lifeExp),pop0=mean(pop),gdp0=mean(gdpPercap)) 
        
        to<-gapminder %>% 
            filter(year==to_year) %>% 
            filter(continent %in% input$continents) %>% 
            group_by(continent) %>% 
            summarize(life1=mean(lifeExp),pop1=mean(pop),gdp1=mean(gdpPercap))
        
        merge(from,to,by="continent") %>% 
            group_by(continent) %>%
            summarize(life_g=((life1-life0)/life0),pop_g=((pop1-pop0)/pop0),gdp_g=((gdp1-gdp0)/gdp0)) %>% 
            mutate(across(where(is.numeric),round,2))%>% 
            kbl(caption="Growth Rate over the years",
                col.names=c("Continent","Life Expectancy","Population","GDP per Capita")) %>% 
            kable_material_dark(full_width = T,html_font = "Cambria",font_size=15) %>% 
            column_spec(2:4, width = "10em")
    }
    
    output$datatable <- DT::renderDataTable({
        gapminder %>% 
            filter(year>=input$years[1] & year<=input$years[2]) %>% 
            filter(continent %in% input$continents) %>% 
            select(year,country,continent,lifeExp) %>% 
            DT::datatable(options = list(pageLength = 10),rownames=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
