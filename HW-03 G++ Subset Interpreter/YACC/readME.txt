### Aşağıdaki 4 tane komut teker teker ve sırasıyla girilerek program çalıştırılır :

# yacc -y -d gpp_interpreter.y
# flex gpp_lexer.l
# gcc gpp_lib.c lex.yy.c y.tab.c -o runInterpreter -g -lm
# ./runInterpreter

NOT ! --> Kod derlenirken warning(ler) verebilir, çalışmasına engel değildir.

NOT ! --> Syntax error verdiğinde programın çalışması durur. 
