##
## EPITECH PROJECT, 2022
## WolfRam
## File description:
## Makefile
##

NAME = wolfram

BUILD =	$(shell stack build)
CLEAN = $(shell stack clean)

PATH := $(shell stack path --local-install-root)
CP = $(shell cp $(PATH)/bin/hpack-exe $(NAME))
RM = $(shell rm -f $(NAME))

$(NAME): 
	$(BUILD)
	@$(CP)

all: $(NAME)

clean:
	$(CLEAN)

fclean: clean
	$(RM)

re: fclean all

.PHONY: all clean fclean re