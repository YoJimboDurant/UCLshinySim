
#This is an about file creator for the application and it is called by ui.R

function(){
  tabPanel("About this Application",
           p(style="text-align:justify",'This online application was developed to simulate substitution methods for left-censored data as compared to NADA'),
           p(style="text-align:justify",'However, because the application has been developed using the ',a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),'package and ',a("R software", href="http://r-project.org/", target="_blank"),'
             the mathematical extensibility is virtually unlimited. We hope that you find this online tool useful for your purposes ! This is free and unencumbered software released into the public domain.

            Anyone is free to copy, modify, publish, use, compile, sell, or
            distribute this software, either in source code form or as a compiled
            binary, for any purpose, commercial or non-commercial, and by any
            means.

            In jurisdictions that recognize copyright laws, the author or authors
            of this software dedicate any and all copyright interest in the
            software to the public domain. We make this dedication for the benefit
            of the public at large and to the detriment of our heirs and
            successors. We intend this dedication to be an overt act of
            relinquishment in perpetuity of all present and future rights to this
            software under copyright law.

            THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
            EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
            MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
            IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
            OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
            ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
            OTHER DEALINGS IN THE SOFTWARE.

            For more information, please refer to <http://unlicense.org/>'),
           br(),

           HTML('<div style="clear: left;"><img src="Hubona-small-jpeg.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
           strong('Author and Application Developer'),
           p('James T Durant',br(),
             'Industrial Hygienist | R Enthusiast',br(),
             a('Check out my blog', href="http://jamestdurant.wordpress.com/", target="_blank"),
             '|',
             a('A Future Hyperlink', href="http://www.google.com", target="_blank")
           ),
           br(),

           div(class="row-fluid",


               div(class="span3", strong('About NADA'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a("NADA", href="http://cran.r-project.org/web/packages/NADA/index.html", target="_blank"),HTML('</li>'),
                     HTML('</ul>')),
                   br()
               ),

               div(class="span3", strong('Educational Resources'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a("Resource A", href="http:www.google.com", target="_blank"),HTML('</li>'),
                     HTML('</ul>')),
                   br()
               ),

               div(class="span3",
                   strong('Abbreviations'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a('ROS: Robust Regression on Order Statistics', href="http://rss.acs.unt.edu/Rdoc/library/NADA/html/ros.html", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a("KM: Kaplan-Meier method (sometimes called reverse Kaplan-Meier)", href="http://rss.acs.unt.edu/Rdoc/library/NADA/html/cenfit.html", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a('MLE: Maximum Likelihood Estimation method', href="http://rss.acs.unt.edu/Rdoc/library/NADA/html/cenmle.html", target="_blank"),HTML('</li>'),

                     HTML('<li>'),a('DL: Detection Limit (censoring value)', href="http:www.google.com", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a('ND: Non-detected (left censored) value', href="http:www.google.com", target="_blank"),HTML('</li>'),
                     HTML('</ul>'))
               )
           )
           )
}
