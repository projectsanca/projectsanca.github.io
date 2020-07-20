#
#	Makefile
#
.POSIX:
.PHONY: all clean help

DATALAKE		:= ./DATALAKE
DATA			:= ./DATA
MAP 			:= ./MAP
FIG 			:= ./FIG

$(MAP)/twalk_map.html: $(DATA)/stats.csv ./map.R
	@./map.R

## make $(DATA)/stats.csv: creates stats CSV
$(DATA)/stats.csv: $(DATA)/cardinal.csv ./stats.R
	@./stats.R

## make $(DATA)/cardinal.csv: creates OD cardinal CSV
$(DATA)/cardinal.csv: $(DATA)/filtered.csv ./cardinal.R
	@./cardinal.R

## make $(DATA)/filtered.csv: creates OD filtered CSV
$(DATA)/filtered.csv: ./demo.data.R
	@./demo.data.R

## make $(FIG)/vis.makefile.png: shows Makefile's DAG
$(FIG)/vis.makefile.png: ./Makefile
	@echo "Visulization of the project's DAG"
	@make -Bnd | make2graph | dot -Tpng -o $@
	@png2ff < $@ > $(FIG)/vis.makefile.ff

## make help: shows this message
help: Makefile
	@sed -n 's/^##//p' $<

## make clean: cleans all
clean:
	@rm -rf $(DATA)/*.*
	@rm -rf $(MAP)/*.*
	@rm -rf $(FIG)/*.*
