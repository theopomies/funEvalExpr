##
## EPITECH PROJECT, 2021
## makefile
## File description:
## wolfram
##

MAKEFLAGS	+=	--no-print-directory -j
BINARY_PATH	=	$(shell stack path --local-install-root)
BINARY_NAME =	funEvalExpr

all:
			stack build
			cp $(BINARY_PATH)/bin/$(BINARY_NAME)-exe ./$(BINARY_NAME)

clean:
			stack clean

fclean: 	clean
			stack purge
			rm -f $(BINARY_NAME)

tests_run: clean
			stack test

re:			fclean all

.PHONY:		all clean fclean re debug test