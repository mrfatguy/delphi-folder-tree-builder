# Files and Folders Tree Builder

Simple tool (with sources), written in Delphi tool to build entire tree of your folders and subfolders.

You can use this project as either a tool, to traverse your entire folders structure, count files and get total file size. Or you can its  source code to implement multi-folder recursive searching algorithm in your own application. Keep in mind, that both comments and names (variables, object) are in Polish. I didn't find enough time and determination to translated them as well. I only tranlsated strings.

**You're only getting project's source code and nothing else! You need to find all missing Delphi components by yourself.**

### Status

Last time `.dpr` file saved in Delphi: **16 November 2007**. Last time `.exe` file built: **12 December 2008**.

**This project ABANDONED! There is no wiki, issues and no support. There will be no future updates. Unfortunately, you're on your own.**

I don't have access to either most of my components used in this or any other of my Delphi projects, nor to Delphi itself. Even translation of this project to English was done by text-editing all `.dfm` and `.pas` files and therefore it may be cracked. It was made in hope to be useful to anyone and for the same reason I'm releasing its source code, but you're using this project or source code at your own responsibility.

Note, that due to my laziness, this project uses core `TTreeView`, knows as buggy and awfully slow. I'm aware of advantages of using super fast solutions like `VirtualTreeView` component, but I was to lazy in implementing it in this project.

### To-do list

- works only in Windows with Polish language, because assumes, that roo folder is called "_Mój komputer_", which is incorrect on non-Polish Windows systems,

- function keys (`Run`, `Rename`, `Delete`, `Open` and `Explore`) does not work, if selected folder contains square (`[]`) or normal (`()`) brackets in its name,

- above mentioned function keys works only on files in listed folders; worth implementing for entire folders as well?

**This project ABANDONED! There is no wiki, issues and no support. There will be no future updates. Unfortunately, you're on your own.**