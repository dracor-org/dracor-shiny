# dracor-shiny

This is the Shiny application for DraCor. 

Learn more about Shiny: https://shiny.rstudio.com

DraCor is a realisation of [Programmable Corpora](https://dracor.org/doc/what-is-dracor) concept. 

This app allows you to interact with the DraCor interactivelly by selecting a play and exploring the respective co-ocurence network, edges and characters data.

You can access the app at https://shiny.dracor.org or locally if you have installed R and RStudio

## How to run DraCor shiny locally

1. Download the project. Ideally: `File -- New Project... -- Version Control -- Git` then put https://github.com/dracor-org/dracor-shiny into the field `Repository URL:` and press `OK`.
2. Install required packages. RStudio will show you a message that some packages are not installed and will give you an option to install them.
3. Press `Run App` button or just run `runApp()` in the console. This will send the app to http://127.0.0.1:(random port) and will listen from it: 
```Listening on http://127.0.0.1:3349.``` The shiny will be opened in a separate window.
