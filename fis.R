# libraries --------------------------------------------------------------------
library(tidyverse)
library(dartR)
library(emR)

# fis and fit ------------------------------------------------------------------

pherm <- emR::ph_filtered
pherm <- gl.keep.loc(pherm, loc.list = locNames(pherm)[1:50])
pop(pherm) <- paste(pherm@other$ind.metrics$gridId, pherm@other$ind.metrics$trip)
pherm2 <- em.gl.max.pop(pherm, s.size = 4, s.less = F)
pop(pherm2) %>% table %>% table

fisgrid <- emR::em.fis.trip(pherm2, fispop = "gridId", nSamp = 4)
nFis <- names(fisgrid[[1]])
fis <- lapply(fisgrid, function(x) x[,1:grep("n", nFis)])

fit <- lapply(fisgrid, 
              function(x) unique(x[,c(grep("trip", nFis),
                                      (grep("Htvar", nFis)-1):length(nFis))]))



fisPeaks <- pbapply::pblapply(fis, function(x) left_join(x, emR::peaks) %>% 
                                mutate(xlog = log(sincePeak +1), x = sincePeak))
mypeaks <- emR::peaks
ft <- left_join(fit[[1]], mypeaks[,c("trip", "sincePeak")])

fp <- fisPeaks[[1]]
ggplot(fp, aes(sincePeak, He)) + geom_point()+
  geom_smooth(method = "lm")
facet_grid(~peakNo)
fisPeaks

fp2 <- filter(fp,complete.cases(fp$capEstimates))
cor(fp$capEstimates, fp$sincePeak)
ggplot(fp, aes(n, He)) + geom_point() + geom_smooth(method = "lm")
lm(capEstimates ~ n, fp) %>% summary

lm(He~ sincePeak + capEstimates, data = fp, weights = varHe) %>% summary

ggplot(ft, aes(sincePeak, fit)) + geom_point() + geom_smooth(method = "lm")
lm(fit ~ sincePeak, data = ft) %>%  summary




library(shiny)
ui <- fluidPage(
  tabsetPanel(
    id="inTabset",
    tabPanel("Tab 1",actionButton("switch_tab", "Go to the third tab")
    ),
    tabPanel("Tab 2", tableOutput("table")),
    tabPanel("Tab 3", "there!"))
  
)

server <- function(input, output, session) {
  
  observeEvent(input$switch_tab, {
    updateTabsetPanel(session, "inTabset",selected = "Tab 2")
  })
  
   output$table <- renderTable({
    head(fis[[1]])
  })
  
}
shinyApp(ui = ui, server = server)
