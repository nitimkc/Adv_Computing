shinyUI(pageWithSidebar(

    headerPanel('LoanData - Decision Boundary Illustration'),

    sidebarPanel(
        numericInput('muApp.Solvency', 'Mean Approved for Solvency', 30, min = 1, max = 300),
        numericInput('stdApp.Solvency', 'Std.Dev Approved for Solvency', 3, min = 1, max = 300),
          numericInput('muApp.PIratio', 'Mean Approved for PI Ratio', 30, min = 1, max = 300),
          numericInput('stdApp.PIratio', 'Std.Dev Approved for PI Ratio', 3, min = 1, max = 300),
        
        numericInput('muDeny.Solvency', 'Mean Approved for Solvency', 30, min = 1, max = 300),
        numericInput('stdDeny.Solvency', 'Std.Dev Approved for Solvency', 3, min = 1, max = 300),
          numericInput('muDeny.PIratio', 'Mean Approved for PI Ratio', 30, min = 1, max = 300),
          numericInput('stdDeny.PIratio', 'Std.Dev Approved for Solvency', 3, min = 1, max = 300)
    ),
    
    mainPanel(
        plotOutput('plot1'),
        tableOutput('table1')
    )
))

