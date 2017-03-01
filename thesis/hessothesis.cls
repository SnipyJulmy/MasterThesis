\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hessothesis}[20/02/2017 MSE Thesis class, version 0.1]
\LoadClass[11pt,a4paper,titlepage,openright]{book}

\RequirePackage{etoolbox}
\RequirePackage{sourcecodepro}
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[english]{babel}

% for bibliography
\RequirePackage[backend=biber,style=chem-angew,sortlocale=en_GB,natbib=true,url=false,doi=true,eprint=false]{biblatex}

% Page style
\RequirePackage{fullpage}
\RequirePackage{fancyhdr}

% Colors & graphics
\RequirePackage[table]{xcolor}
\RequirePackage[pdftex]{graphicx}

\RequirePackage{titlesec}
\RequirePackage[chapter]{minted}
\RequirePackage{amsmath}

\RequirePackage[toc,page]{appendix}

\RequirePackage{parskip}

\RequirePackage{csquotes}

\RequirePackage{glossaries}
\makeglossaries

\RequirePackage[margin=2.5cm, left=2.5cm, right=2.5cm,
twoside=true,includehead,includefoot,heightrounded,headsep=1cm]{geometry}

\let\@ThesisTitle\@empty
\let\@Author\@empty
\let\@Supervisor\@empty
\let\@SupervisorResearchUnit\@empty
\let\@ExternalExpert\@empty
\let\@ExternalExpertResearchUnit\@empty
\let\@Place\@empty
\let\@Date\@empty
\let\@AuthorEmail\@empty
\let\@Orientation\@empty

% Cross-references
\RequirePackage{hyperref}

\def\@mselogo{\includegraphics[width=0.8\textwidth]{img/mse_logo}}
\def\@hessologo{\includegraphics[scale=0.7]{img/logo_hes-so}}

\newcommand{\ThesisTitle}[1]{\gdef\@ThesisTitle{#1}}
\newcommand{\Author}[1]{\gdef\@Author{#1}}
\newcommand{\Supervisor}[1]{\gdef\@Supervisor{#1}}
\newcommand{\SupervisorResearchUnit}[1]{\gdef\@SupervisorResearchUnit{#1}}
\newcommand{\ExternalExpert}[1]{\gdef\@ExternalExpert{#1}}
\newcommand{\ExternalExpertResearchUnit}[1]{\gdef\@ExternalExpertResearchUnit{#1}}
\newcommand{\Place}[1]{\gdef\@Place{#1}}
\newcommand{\Date}[1]{\gdef\@Date{#1}}
\newcommand{\AuthorEmail}[1]{\gdef\@AuthorEmail{#1}}
\newcommand{\Orientation}[1]{\gdef\@Orientation{#1}}

\newcommand{\HRule}{\rule{\linewidth}{0.5mm}}

% Scala
\newminted{scala}{frame=single, framesep=6pt, breaklines=true, fontsize=\scriptsize}
\newmintedfile{scala}{frame=single, framesep=6pt, breaklines=true,
  fontsize=\scriptsize}

\setlength{\headheight}{17pt}
\pagestyle{fancy}

% Standard pages (inside chapters)
\fancyhf{}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0pt}
\fancyheadoffset{0cm}

\fancyhead[OR]{\bfseries \nouppercase{\rightmark}}
\fancyhead[EL]{\bfseries \nouppercase{\leftmark}}
\fancyfoot[EL,OR]{\thepage}


% First page of chapters
\fancypagestyle{plain}{
	\fancyhf{}
	\renewcommand{\headrulewidth}{0pt}
	\renewcommand{\footrulewidth}{0pt}
	\fancyfoot[EL,OR]{\thepage}
}

% Use empty style for page when clearing double pages
\def\cleartoodd{%
	\clearpage%
	\ifodd\value{page}\else\mbox{}\thispagestyle{empty}\newpage\fi%
}

\def\clearchap{%
	\ifodd\value{page}\else\mbox{}\thispagestyle{empty}\fi%
}

\newcommand{\makeinfopage}{Info page}

% \cleardoublepage replaced by \cleartoodd
\let\origdoublepage\cleardoublepage
\renewcommand{\cleardoublepage}{%
	\cleartoodd%
}

\newenvironment{alwayssingle}{%
       \@restonecolfalse
       \if@twocolumn\@restonecoltrue\onecolumn
       \else\if@openright\cleardoublepage\else\clearpage\fi
       \fi}%
       {\if@restonecol\twocolumn
       \else\newpage\thispagestyle{empty}\fi}

\renewcommand\maketitle{
  \begin{alwayssingle}
    {\begin{titlepage}
        \begin{flushright}
          \begin{minipage}{0.49\textwidth}
            \begin{flushleft}
              \par \includegraphics[width=1.0\textwidth]{img/mse_logo} \par
            \end{flushleft}
          \end{minipage}
          \hfill
          \begin{minipage}{0.49\textwidth}
            \begin{flushright}
              \par \includegraphics[width=0.8\textwidth]{img/logo_hes-so} \par
            \end{flushright}
          \end{minipage}
          \begin{flushleft}
            \footnotesize
            Master of Science HES-SO in Engineering \\
            Av. de Provence 6 \\
            CH-1007 Lausanne
          \end{flushleft}
          {\vspace*{0.5cm}}
          {\Huge \par Master of Science HES-SO in Engineering \par \vspace*{0.5cm}}
          {\LARGE Orientation: \@Orientation \vspace*{1.5cm}}
          {\Huge \par \@ThesisTitle \par \vspace*{1.5cm}}
          {\large Author:\\[-0.1cm] \Huge \par \@Author \par \vspace*{0.8cm}}
          {\large Under the direction of:}
          {\Large \par \@Supervisor \par}
          {\par \@SupervisorResearchUnit \par \vspace*{0.5cm}}
          {\large External expert: \par \@ExternalExpert \par}
          {\vfill}
          % Bottom of the page
          {\large \par \@Place , HES-SO//Master, \@Date \par}
        \end{flushright}
        {\restoregeometry}
      \end{titlepage}}
  \end{alwayssingle}
{\newgeometry{left=3.5cm,right=3.5cm,top=3cm,bottom=3cm}}
}
\endinput

%%% Local Variables:
%%% TeX-command-extra-options: "-shell-escape"
%%% mode: latex
%%% TeX-master: "thesis"
%%% End: