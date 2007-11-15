all: backups archive findcopies

SSH=ssh -o 'PreferredAuthentications hostbased,publickey'
SCP=scp -o 'PreferredAuthentications hostbased,publickey'

backups: Main.hs Makefile URI.hs Backup.hs
	ghc6 --make -fglasgow-exts -W -O2 Main.hs -o $@
	$(SCP) backups david@server:/usr/lib/cgi-bin

archive: ArchiveMain.hs Archive.hs Makefile
	ghc6 --make -fglasgow-exts -W -O2 ArchiveMain.hs -o $@
	strip $@
	set -x && for addr in 192.168.0.2 192.168.0.3 192.168.0.108; \
	  do $(SCP) $@ root@$$addr:/srv/backups/$@ && \
	     $(SSH) root@$$addr 'chown root.root /srv/backups/$@ && chmod 4755 /srv/backups/$@'; done

findcopies : FindCopiesMain.hs FindCopies.hs
	ghc6 --make -fglasgow-exts -W -O2 FindCopiesMain.hs -o $@
	strip $@
	set -x && for addr in 192.168.0.2 192.168.0.3 192.168.0.108; \
	  do $(SCP) $@ root@$$addr:/srv/backups/$@ && \
	     $(SSH) root@$$addr 'chown root.root /srv/backups/$@ && chmod 4755 /srv/backups/$@'; done

clean:
	rm -f *.hi *.o backups
