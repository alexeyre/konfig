{config, pkgs, lib, ... }:
{	
	programs.emacs = {
		enable = true;
		#package = pkgs.emacsGit;
	};
	# services.emacs.enable = true;
 	# home.file.emacs = {
		# source = "${dotfilesGit}/emacs/";
		# target = ".emacs.d/";
		# recursive = true;
	# };
}
