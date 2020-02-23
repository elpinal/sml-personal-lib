MLKIT := mlkit

mlkit:
	$(MLKIT) personal.mlb

mlton:
	mlton personal.mlb

smlnj:
	sml -m personal.cm

alice:
	alicec --no-warn-conventions --no-warn-unused-imports --dependency-file project.depend std.sml nat.sml queue.sml non_empty.sml ordered.sml map.sml bst.sml

.PHONY: mlkit mlton smlnj alice
