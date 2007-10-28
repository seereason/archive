all: backups archive findcopies

SSH=ssh -o 'PreferredAuthentications hostbased,publickey'
SCP=scp -o 'PreferredAuthentications hostbased,publickey'

backups: Main.hs Makefile URI.hs Backup.hs
	ghc6 --make -fglasgow-exts -W -O2 Main.hs -o $@
	$(SCP) backups david@server:/usr/lib/cgi-bin

archive: ArchiveMain.hs Archive.hs Makefile
	ghc6 --make -fglasgow-exts -W -O2 ArchiveMain.hs -o $@
	strip archive
	$(SCP) archive root@server:/srv/backups/archive
	set -x && for addr in 192.168.0.2 192.168.0.3; \
	  do $(SCP) archive root@$$addr:/srv/backups/archive && \
	     $(SSH) root@$$addr 'chown root.root /srv/backups/archive && chmod 4755 /srv/backups/archive'; done

findcopies : FindCopies.hs
	ghc6 --make -fglasgow-exts -W -O2 FindCopies.hs -o $@

clean:
	rm -f *.hi *.o backups
