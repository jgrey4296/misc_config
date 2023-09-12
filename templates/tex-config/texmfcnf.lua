-- (Public domain.)
-- Symlink into /usr/local/texlive/YYYY/
--
-- This texmfcnf.lua file should contain only your personal changes from the
-- original texmfcnf.lua (for example, as chosen in the installer).
--
-- That is, if you need to make changes to texmfcnf.lua, put your custom
-- settings in this file, which is .../texlive/YYYY/texmfcnf.lua, rather than
-- the distributed file (.../texlive/YYYY/texmf-dist/web2c/texmfcnf.lua).
-- And include *only* your changed values, not a copy of the whole thing!

return {
  content = {
    variables = {
      TEXMFLOCAL = "selfautoparent:/texmf-local",
      TEXMFHOME = "~/Library/texmf",
      TEXMFVAR = "~/Library/texlive/2023basic/texmf-var",
      TEXMFCONFIG = "~/Library/texlive/2023basic/texmf-config",
    },
  },
}
