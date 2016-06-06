# Wordcount

this is a boot script
 https://github.com/boot-clj/boot

to use :
 - install boot: https://github.com/boot-clj/boot#install
 - chmod +x wordcount.clj
 - get the help: ./wordcount.clj --help
 - sample use: ./wordcount.clj test/resources/war-and-peace-full-book.txt
 - using named options: ./wordcount.clj --parallel -i test/resources/war-and-peace-full-book.txt
 - using named options: ./wordcount.clj --all -i test/resources/war-and-peace-full-book.txt

#Develop

 - boot repl (boot repl wait if wanting to connect via cider - may have to enter the port manually))
 - (load-file "wordcount.clj")
 - comments forms left for experimenting
