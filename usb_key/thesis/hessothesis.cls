\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hessothesis}[20/02/2017 MSE Thesis class, version 0.1]
\LoadClass[11pt,a4paper,titlepage,openright]{book}

\RequirePackage{etoolbox}
\RequirePackage{sourcecodepro}
\RequirePackage[T1]{fontenc}
\RequirePackage[english]{babel}

% for bibliography
\RequirePackage[
  backend=biber,
  style=alphabetic,
  sortlocale=en_GB,
  natbib=true,
  url=false,
  doi=true,
  eprint=false,
  citestyle=alphabetic-verb
  ]{biblatex}

% Page style
\RequirePackage{fancyhdr}

% Colors & graphics
\RequirePackage[table]{xcolor}
\RequirePackage{graphicx}

\RequirePackage{titlesec}
\RequirePackage[chapter,cache=false]{minted}
\RequirePackage{amsmath}

\RequirePackage[toc,page]{appendix}

\RequirePackage{parskip}

\RequirePackage{float}
\RequirePackage{scrextend}

\RequirePackage{csquotes}

% Algorithm
\RequirePackage{algorithm}
\RequirePackage{algpseudocode}

\RequirePackage[above,below,section]{placeins}

\RequirePackage{glossaries}
\makeglossaries

\RequirePackage[margin=2.5cm, left=2.5cm, right=2.5cm,
twoside=true,includehead,includefoot,heightrounded,headsep=1cm]{geometry}

\vbadness=10000
\widowpenalties 1 10000
\raggedbottom

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
\let\@PhoneNumber\@empty

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
\newcommand{\PhoneNumber}[1]{\gdef\@PhoneNumber{#1}}


\newcommand{\HRule}{\rule{\linewidth}{0.5mm}}

%% Minted settings
\usemintedstyle{tango}

% Scala
\newminted{scala}{frame=single, framesep=6pt, breaklines=true, fontsize=\scriptsize}
\newmintedfile{scala}{
  frame=single,
  framesep=6pt,
  breaklines=true,
  fontsize=\scriptsize}
\newmintinline{scala}{breaklines=true,fontsize=\small}
\newminted[inlinescala]{scala}{breaklines=true,fontsize=\small}

% Arduino
\newminted{arduino}{frame=single, framesep=6pt, breaklines=true, fontsize=\scriptsize}
\newmintedfile{arduino}{frame=single, framesep=6pt, breaklines=true,
  fontsize=\scriptsize}
\newmintinline{arduino}{breaklines=true,fontsize=\small}

% Mbed
\newminted{cpp}{frame=single, framesep=6pt, breaklines=true, fontsize=\scriptsize}
\newmintedfile{cpp}{frame=single, framesep=6pt, breaklines=true,
  fontsize=\scriptsize}
\newmintinline{cpp}{breaklines=true,fontsize=\small}

% Apdl
\newminted[apdlcode]{python}{frame=single, framesep=6pt, breaklines=true,
  fontsize=\scriptsize}
\newmintinline[apdlinline]{python}{fontsize=\small,breaklines=true}
\newminted[inlineapdl]{python}{fontsize=\small,breaklines=true}

% Python
\newminted{python}{frame=single, framesep=6pt, breaklines=true, fontsize=\scriptsize}

% EBNF
\newminted{ebnf}{frame=single, framesep=6pt,
  breaklines=true,fontsize=\scriptsize,style=bw}
\newmintinline{ebnf}{fontsize=\small,style=bw}
\newminted[inlineebnf]{ebnf}{fontsize=\small,style=bw}

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

% \graphicspath{{./img/}}

\newcommand{\TestExpectedResult}[3]
{
  \begin{tabular}{|l|c|r|}
    \hline
    \gls{APDL} code & expected code & generated code \\
    \hline
    #1 & #2 & #3 \\
    \hline
  \end{tabular}
}

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

\newcommand{\namesigdate}[2][6cm]{%
\begin{minipage}{#1}
    #2 \vspace{1.33cm}\hrule\smallskip
    \footnotesize \textit{Signature}
    \vspace{1.33cm}\hrule\smallskip
    \footnotesize \textit{Location, date}
\end{minipage}
}

\newcommand{\makeinfopage}{
    \cleardoublepage
    \newgeometry{top=37mm, bottom=42mm, left=36mm, right=29mm, headheight=14pt}

    \chapter*{Information}
    \markboth{Information}{Information}
    \addcontentsline{toc}{chapter}{Information}

    \vspace{2em}

    %=====================================================
    \section*{Contact Information}

    \begin{tabbing}
    \'Author: \= \@Author\\
          \> Graduate student - Computer Science\\
          \> University of Applied Sciences Western Switzerland (HES-SO)\\\\

    \'Phone:  \> \@PhoneNumber\\\\

    \'Email:    \> \href{mailto:\@AuthorEmail}{\@AuthorEmail} 
    \end{tabbing}

    \vspace{2em}

    %=====================================================
    \section*{Declaration of Honor}

    I hereby solemnly declare that I have personally and independently prepared
    this paper. All quotations in the text have been marked as such, and the
    paper or considerable parts of it have not previously been subject to any
    examination or assessment.
    
    \vspace{2em}

    \noindent\namesigdate{\@Author}\hfill

    \vspace{2em}

    \section*{Validation}
    %=====================================================

    \noindent Accepted by the HES-SO\thinspace//\thinspace Master (Switzerland, Lausanne) on a proposal from:

    \vspace{3em}

    \noindent\namesigdate{Prof. \@Supervisor \\Thesis project advisor}
    \hfill\namesigdate{Prof. Pierre Pompili\\Departement director}

    \restoregeometry
}

\newcommand{\newChapter}[2]{
  {
    \titleformat{\chapter}[display]
    {\normalfont\Large\filcenter\sffamily}
    {\titlerule[1pt]%
    \vspace{1pt}%
    \titlerule
    \vspace{1pc}%
    \LARGE\MakeUppercase{\chaptertitlename} \thechapter}
    {1pc}
    {
      \titlerule
      \vspace{1pc}%
      \Huge
    }
    \chapter{#1}
    \label{#2}
    \newpage
  }
}

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
