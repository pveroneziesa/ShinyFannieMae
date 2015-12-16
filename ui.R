require("shiny")



shinyUI(fluidPage(theme = "bootstrap_space.css",
  
  navbarPage("Fannie Mae Analysis",collapsible = TRUE,inverse=TRUE,
             tabPanel("About the Project",
                      fluidRow(
                        column(12,tabsetPanel(
                          tabPanel("Fannie Mae",
                                   h4("A little about the Fannie Mae history",align="center"),
                                   br(),
                                   br(),
                                   p("The Federal National Mortgage Association (FNMA), colloquially known as Fannie Mae, was established in 1938 by amendments to the National Housing Act after the Great Depression as part of Franklin Delano Roosevelt's New Deal. Fannie Mae was established to provide local banks with federal money to finance home mortgages in an attempt to raise levels of home ownership and the availability of affordable housing. Fannie Mae created a liquid secondary mortgage market and thereby made it possible for banks and other loan originators to issue more housing loans, primarily by buying Federal Housing Administration (FHA) insured mortgages. For the first thirty years following its inception, Fannie Mae held a monopoly over the secondary mortgage market."),
                                   br(),
                                   p("It was acquired by the Housing and Home Finance Agency from the Federal Loan Agency as a constituent unit in 1950. In 1954, an amendment known as the Federal National Mortgage Association Charter Act made Fannie Mae into 'mixed-ownership corporation' meaning that federal government held the preferred stock while private investors held the common stock; in 1968 it converted to a privately held corporation, to remove its activity and debt from the federal budget. In the 1968 change, arising from the Housing and Urban Development Act of 1968, Fannie Mae's predecessor (also called Fannie Mae) was split into the current Fannie Mae and the Government National Mortgage Association ('Ginnie Mae')."),
                                   br(),
                                   p("Ginnie Mae, which remained a government organization, supports FHA-insured mortgages as well as Veterans Administration (VA) and Farmers Home Administration (FmHA) insured mortgages. As such, Ginnie Mae is the only home-loan agency explicitly backed by the full faith and credit of the United States government"),
                                   br(),
                                   p("In 1970, the federal government authorized Fannie Mae to purchase conventional mortgages, the same year it went public on New York and Pacific Exchanges, (Residential Mortgage Loan Origination 2nd edition pg.8, J.Keith Baker 2010), i.e. those not insured by the FHA, VA, or FmHA, and created the Federal Home Loan Mortgage Corporation (FHLMC), colloquially known as Freddie Mac, to compete with Fannie Mae and thus facilitate a more robust and efficient secondary mortgage market."),
                                   br(),
                                   p("In 1981, Fannie Mae issued its first mortgage passthrough and called it a mortgage-backed security. The Fannie Mae laws did not require the Banks to hand out subprime loans in any way. Ginnie Mae had guaranteed the first mortgage passthrough security of an approved lender in 1968 and in 1971 Freddie Mac issued its first mortgage passthrough, called a participation certificate, composed primarily of private mortgages.")
                                   
                          ),
                          tabPanel("Data Base",
                                   h4("Fannie Mae's Data Base", align="center"),
                                   br(),
                                   br(),
                                   p("Based on previous section, there are different methods to obtain the raw data from Fannie Mae datasets. We will go through two methods in this section. One is to download data from the website Load performance data. The loan performance data is divided into two text files: acquisition and performance data text files."),
                                   br(),
                                   p("However, although it is possible to clean the dataset in various statistic tools, we have easier method to obtain dataset: Accessing the postgreSQL database and query through all the data, to get the only fields that we need from both tables. For this section, the connection is simple and clean by one functions in R.	The next step is to define tables and variables for saving.")
                                   
                                   
                                   ),
                          tabPanel("Research Project",p("summary about the research project, its assumptions etc")),
          
                          tabPanel("What is Machine Learning",
                                   h4("Neural Network",align="center"),
                                   br(),
                                   br(),
                                   p("Neural networks are a set of algorithms, modeled loosely after the human brain, that are designed to recognize patterns. They interpret sensory data through a kind of machine perception, labeling or clustering raw input. The patterns they recognize are numerical, contained in vectors, to which all real-world data, be it images, sound, text or time series, must be translated."),
                                   br(),
                                   p("Neural network is composed of several node layers. A node is a place where computation happens, loosely patterned on the human neuron and firing when sufficient stimuli pass through. It combines input from the data with a set of coefficients, or weights, that either amplify or mute that input. These input-weight products are summed and the sum is passed through a node's so-called activation function."),
                                   br(),
                                   br(),
                                   h4("Deep Learning", align="center"),
                                   br(),
                                   br(),
                                   p("Deep learning uses neural networks (DNNs) many layers deep and large datasets to teach computers how to solve perceptual problems, such as detecting recognizable concepts in data, translating or understanding natural languages, interpreting information from input data, and more. Deep learning is used in the research community and in industry to help solve many big data problems such as computer vision, speech recognition and natural language processing."),
                                   br(),
                                   p("I have to mention that one of the features is somewhat mysterious, although the neural network gives impressive performance. The weights and biases in the network were discovered automatically. In other words, how the network does what it does can not be explained immediately."),
                                   br(),
                                   p("Deep learning is the fastest growing area of machine learning. Deep learning uses convolutional neural networks to learn many levels of abstraction. The levels of abstractions range from simple concepts to complex, the more complex require more layers in your network. Each Layer categorizes some kind of information, refines it and passes it along to the next. These many layers are what put the deep into deep learning.")
                                   ),
                                       
                          tabPanel("Variables",
                                   br(),
                                   p("The variables used for this Project were as follow:"),
                                   br(),
                                   strong("Seller Name:"),
                                   p("The name of the entity that delivered the mortgage loan to Fannie Mae."),
                                   br(),
                                   strong("Original Interest Rate: "),
                                   p("The original interest rate on a mortgage loan as identified in the original mortgage loan documents."),
                                   br(),
                                   strong("Original Loan-to-Value: "),
                                   p("A ratio calculated at the time of origination for a mortgage loan. The Original LTV reflects the loan-to-value ratio of the loan amount secured by a mortgaged property on the origination date of the underlying mortgage loan."),
                                   br(),
                                   strong("Debt-to-Income Ratio: "),
                                   p("A ratio calculated at origination derived by dividing the borrower's total monthly obligations (including housing expense) by his or her stable monthly income. This calculation is used to determine the mortgage amount for which a borrower qualifies"),
                                   br(),
                                   strong("Borrower Score: "),
                                   p("A numerical value used by the financial services industry to evaluate the quality of borrower credit. Credit scores are typically based on a proprietary statistical model that is developed for use by credit data repositories. These credit repositories apply the model to borrower credit information to arrive at a credit score. When this term is used by Fannie Mae, it is referring to the classic FICO score developed by Fair Isaac Corporation."),
                                   br(),
                                   strong("3 Digit ZipCode:"),
                                   p("The code designated by the U.S. Postal Service which indicates the geographical area where the subject property is located."),
                                   br(),
                                   br()
                                   
                                   ),
                          tabPanel("Model Results",
                                   br(),
                                   fluidRow(
                                     
                                     column(8,
                                       fluidRow(
                                         br(),
                                         h3(strong("Results"),align="center"),
                                         br(),
                                         h4(strong("Train data:")),
                                         p("######Missclassification rate of 77.71462%, on the train dataset."),
                                         h4(strong("Valid data:")),
                                         p("#######Missclassification rate of 77.71462%, on the valid dataset."),
                                         h4(strong("Test data:")),
                                         p("Missclassification rate of 77.71462%, on the test dataset, in which the model never saw before."),
                                         p("For each model: GLM - 75.93061%, Random Forest - 78.60080%, GBM - 77.08559%, Deep Learning - 76.98882%"),
                                         br(),
                                         p("This model uses the Ensemble Technique, which is use multiple models to obtain better predictive performance than could be obtained from any of the constituent models."),
                                         p("Generalized Model Stacking (combine the predictions from multiple models)"),
                                         p("If your set of base learners does not contain the true prediction function, ensembles can give a good approximation of that function. Ensembles perform better than the individual base algorithms. Ensembles are better than Grid Search"),
                                         p("The Super Learner algorithm is a loss-based supervised learning method that finds the optimal combination of a collection of prediction algorithms."),
                                         p("Super Learner performs asymptotically as well as the best possible weighted combination of the base learners")
                                       )
                                     ), 
                                     
                                     column(4,
                                            fluidRow(
                                              br(),
                                              h5(strong("Try here some predictions:"), align="center")
                                            ),
                                            fluidRow(
                                              column(6,
                                                     
                                                     selectInput("pred_select_zip",label="3Dig Zip",
                                                                 choices = pred_zip_data,
                                                                 selected = 1),
                                                     sliderInput("pred_year", label = h6("YEAR", align="center"),
                                                                 min = 2000, max = 2014, value = 2005),
                                                     sliderInput("pred_bcs", label = h6("Borrower Score", align="center"),
                                                                 min = 500, max = 800, value = 600)
                                                     ),
                                              column(6,
                                                     
                                                     sliderInput("pred_olv", label = h6("Loan-to-Value", align="center"),
                                                                 min = 0, max = 100, value = 50),
                                                     sliderInput("pred_dir", label = h6("Debt-to-Income", align="center"),
                                                                 min = 0, max = 65, value = 50),
                                                     sliderInput("pred_oir", label = h6("Interest Rate", align="center"),
                                                                 min = 0, max = 15, value = 8)
                                                     
                                                     )
                                            ),
                                            fluidRow(
                                              selectInput("pred_select_banks",label="pred_banks",
                                                          choices = pred_banks_data,
                                                          selected = 1)
                                            )
                                            )
                                     
                                   )
                          
                                   )

                        ))
                      )
                      ),
             tabPanel("Visualization",
                      
                      fluidRow(
                        h5("For get the best experience on this visualization, one should change all the setting for the graphs as follow and the press the button to update the view"),
                        br(),
                        p("Sorry for the time consuming, but just remember you are working with a ~22M entries for 8 variables, so it is natural to take a while"),
                        br(),
                        submitButton("Update View")
                        
                      ),
                      
                      
                      fluidRow(
                        plotOutput("yearly"),
                        
                        sliderInput("year_slide",label=h6("YEAR"),
                                    min = 2000, max = 2014, value = c(2000,2004),
                                    width="100%"
                                    )
                      ),
                      
                      fluidRow(
                        
                        column(8,
                               fluidRow(
                                 
                                 plotOutput("map")
                               ),
                               
                               fluidRow(
                                 column(6,
                                        plotOutput("bcs_hist"),
                                        br(),
                                        sliderInput("bcs_slider",label=h6("Borrower Score"),
                                                    min=500,max=800,value=c(550,650),
                                                    width="100%"
                                                    )
                                        ),
                                 column(6,
                                        plotOutput("olv_hist"),
                                        br(),
                                        sliderInput("olv_slider",label=h6("Loan-to-Value"),
                                                    min=1,max=100,value=c(70,90),
                                                    width="100%"
                                                    )
                                        )
                               ),
                               
                               fluidRow(
                                 column(6,
                                        plotOutput("dir_hist"),
                                        br(),
                                        sliderInput("dir_slider",label=h6("Debt-to-Income"),
                                                    min=1,max=65,value=c(45,60),
                                                    width="100%"
                                        )
                                 ),
                                 column(6,
                                        plotOutput("oir_hist"),
                                        br(),
                                        sliderInput("oir_slider",label=h6("Original Interest Rate"),
                                                    min=0.01,max=15,value=c(4,9),
                                                    width="100%"
                                        )
                                 )
                               )
                        ),
                        
                        column(4,
                               
                               #uiOutput("select_banks"),
                               checkboxInput("checkbox_banks",label="All Banks",
                                             value=TRUE
                               ),
                               uiOutput("banks"),
                               br(),
                               plotOutput("bank_vertical")
                               
                               )
                        
                      )
                      
                      ),
             navbarMenu("More",
                      tabPanel("People"))
             )
  

))