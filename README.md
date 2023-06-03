# Football-Match-Statistics


This is a Shiny web application that allows you to input and track match statistics for football matches. It provides a user-friendly interface to enter the details of each match, including the teams, goals scored, and shots taken. The application also calculates and displays the league standings based on the accumulated statistics.

# Installation

To run the application locally, you need to have R and the required packages installed. You can install the necessary packages by running the following commands in your R console:

- install.packages("shiny")
- install.packages("shinydashboard")

# Usage
To launch the application, open the R script file (app.R) and execute the shinyApp(ui = ui, server = server) command. This will start the Shiny server, and you can access the application through your web browser.

The application consists of two tabs:

- Input: Allows you to enter the match statistics, including the team names, goals scored, and shots taken.
- Table: Displays the match statistics table, league standings, and the winner of the match.

# Required Changes
- Does not updates the wins,loss and draw columns.
- Winner is not displayed Correctly.

# Contributing
If you find any issues or have suggestions for improvement, please feel free to open an issue or submit a pull request. Contributions are welcome!

# Contact
If you have any questions or suggestions, please feel free to contact Me.
You can customize the content as per your project's specific details and requirements.
