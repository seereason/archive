all: backups archive

backups: Main.hs Makefile URI.hs Backup.hs
	ghc6 --make -fglasgow-exts -W -O2 Main.hs -o $@
	scp backups david@server:/usr/lib/cgi-bin/

archive: ArchiveMain.hs Archive.hs Makefile
	ghc6 --make -fglasgow-exts -W -O2 ArchiveMain.hs -o $@

clean:
	rm -f *.hi *.o backups
