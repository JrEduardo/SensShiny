        output$FOOTER <- renderPrint({
            l1 <- paste(\"<code>SensShiny</code>: R Package version\",
                        packageVersion(\"SensShiny\"))
            l2 <- paste0(\"URL: <a target=\\"_blank\\"\",
                         \"href=\\"https://github.com/\",
                         \"JrEduardo/SensShiny\\">\",
                         \"https://github.com/JrEduardo/SensShiny</a>\")
            l3 <- paste0(\"Contact: <a \",
                         \"href=\\"mailto:edujrrib@gmail.com\\">\",
                         \"edujrrib@gmail.com</a>\")
            cat(\"<div class=\\"footer\\">\", l1, l2, l3, 
                \"<br/></div>\", sep = \"<br/>\")
        })
