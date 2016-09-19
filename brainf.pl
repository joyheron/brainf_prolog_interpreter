:- set_prolog_flag(verbose, silent).

:- use_module(interpreter, [exec/1]).

:- initialization main.

run([File|_OtherArgs]) :-
	exec(File).

main :-
    current_prolog_flag(argv, Argv),
    run(Argv),
    halt.
main :-
    halt(1).