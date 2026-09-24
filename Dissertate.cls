% -------------------------------------------------------------------
%  @LaTeX-class-file{
%     filename        = "Dissertate.cls",
%     version         = "2.0",
%     date            = "25 March 2014",
%     codetable       = "ISO/ASCII",
%     keywords        = "LaTeX, Dissertate",
%     supported       = "Send email to suchow@post.harvard.edu.",
%     docstring       = "Class for a dissertation."
% --------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Dissertate}[2014/03/25 v2.0 Dissertate Class]
\LoadClass[12pt, oneside, letterpaper]{book}

% Text layout.
\RequirePackage[ 
  letterpaper,
  left=1in,
  right=1in,
  top=1in,
  bottom=1in]{geometry}
\usepackage{ragged2e}
\RaggedRight
\RequirePackage{graphicx}
\usepackage{subcaption}
\parindent 12pt
\RequirePackage{lettrine}
\RequirePackage{setspace}
\RequirePackage{verbatim}
\RequirePackage{fvextra}

\RequirePackage{amssymb}
\usepackage[fleqn]{amsmath}
% Avoid the legacy physics package, which is not required by this document and
% can interfere with modern math tagging. These two forms cover its uses here.
\providecommand{\matrixelement}[3]{\left\langle #1\middle|#2\middle|#3\right\rangle}
\providecommand{\mel}[3]{\left\langle #1\middle|#2\middle|#3\right\rangle}
\DeclareMathOperator{\sech}{sech}

% Paragraph layout (kept explicit so sectioning commands remain untouched
% for native PDF tagging).
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.5\baselineskip plus 2pt}

% Fonts.
% UTF-8 is native in current LaTeX; retained for compatibility with pdfLaTeX.
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{lmodern}
% Use one text font family throughout the dissertation.
\renewcommand{\familydefault}{\rmdefault}
%\usepackage[english]{babel}
\usepackage{blindtext}
\RequirePackage{color}
\RequirePackage{url}
\widowpenalty=300
\clubpenalty=300
\renewcommand{\thefootnote}{\fnsymbol{footnote}}

% Headings and headers.
\RequirePackage{fancyhdr}

% UNLV consistency: format chapters without titlesec.
% Keeping the standard book-class \chapter machinery allows LaTeX's native
% PDF tagging code to create and close section structures correctly.
\renewcommand{\@makechapterhead}[1]{%
  \vspace*{0pt}%
  {\parindent\z@\raggedright\normalfont\normalsize\bfseries
    \ifnum\c@secnumdepth>\m@ne
      \ifnum\value{chapter}>0\relax
        \chaptername\space\thechapter\par
        \vskip .5\baselineskip
      \fi
    \fi
    #1\par\nobreak
    \vskip 1.5\baselineskip}}
\renewcommand{\@makeschapterhead}[1]{%
  \vspace*{0pt}%
  {\parindent\z@\raggedright\normalfont\normalsize\bfseries
    #1\par\nobreak
    \vskip 1.5\baselineskip}}
\setlength{\headheight}{15pt}
\pagestyle{plain}
\RequirePackage{titling}

%citations
\usepackage[style=numeric,backend=bibtex]{biblatex}
\addbibresource{dissertation.bib}
%$\usepackage[breaklinks=true]{hyperref}
%\usepackage{breakcites}
%\usepackage{url}
%\usepackage{cite}
%\usepackage[super]{natbib}
%\usepackage{notoccite}
%\usepackage{doi}
%\bibliographystyle{unsrt}

%math
\usepackage{amssymb}
\usepackage{braket}
\usepackage{mathtools}

%chemistry
\usepackage{leftidx}

%table
\usepackage{hhline}
\usepackage{array}
\usepackage{graphicx}
\usepackage{rotating}
%\usepackage{pdflscape}
\usepackage{afterpage}
\usepackage{caption}
% geometry was already loaded above; loading it twice causes option clashes.
\usepackage{float}

% Add source code
\usepackage{listings}

% Add pdf file
\usepackage{pdfpages} 

% Front matter.
\setcounter{tocdepth}{1}
\usepackage[titles]{tocloft}
\usepackage{appendix}
\renewcommand{\cftsecleader}{\cftdotfill{\cftdotsep}}
\renewcommand{\cftchapfont}{\normalfont\normalsize}
\renewcommand{\cftchappresnum}{Chapter\space}
\renewcommand{\cftchapaftersnum}{}
\setlength{\cftchapnumwidth}{6.5em}
\renewcommand\listfigurename{List of Figures}
\renewcommand\listtablename{List of Tables}

% Some definitions.
%\def\degreeyear#1{\gdef\@degreeyear{#1}}
%\def\degreemonth#1{\gdef\@degreemonth{#1}}
\def\advisor#1{\gdef\@advisor{#1}}
%\def\department#1{\gdef\@department{#1}}
%\def\degree#1{\gdef\@degree{#1}}
%\def\field#1{\gdef\@field{#1}}
%\def\pdOneName#1{\gdef\@pdOneName{#1}}
%\def\pdOneSchool#1{\gdef\@pdOneSchool{#1}}
%\def\pdOneYear#1{\gdef\@pdOneYear{#1}}
%\def\pdTwoName#1{\gdef\@pdTwoName{#1}}
%\def\pdTwoSchool#1{\gdef\@pdTwoSchool{#1}}
%\def\pdTwoYear#1{\gdef\@pdTwoYear{#1}}

\usepackage{hyperref}

% Accessible heading hierarchy for tagged PDF.
% The cover title is explicitly tagged as H1 in cover.tex.
% UNLV requires major sections/chapters as H2, first-level headings as H3, etc.
% Keep LaTeX's native sectioning structures, but shift the heading roles down one level.
% sec/0 = chapter (also chapter* front-matter headings), sec/1 = section, etc.
\AssignStructureRole{sec/0/title}{H2}
\AssignStructureRole{sec/1/title}{H3}
\AssignStructureRole{sec/2/title}{H4}
\AssignStructureRole{sec/3/title}{H5}
\AssignStructureRole{sec/4/title}{H6}
% Do not add cmd/chapter or cmd/section tagstruct hooks here.

% Improve bookmark hierarchy for unnumbered front-matter headings.
\hypersetup{unicode=true,bookmarksnumbered=true,bookmarksopen=true,hidelinks}

% Display bibliography URLs as plain text, not clickable hyperlinks.
\DeclareFieldFormat{url}{\nolinkurl{#1}}

\newcommand{\copyrightpage}{
  \newpage
  \thispagestyle{empty}
  \vspace*{\fill}
  \normalfont\normalsize \noindent \copyright 2023\hspace{3pt}-- \theauthor \\
  \noindent all rights reserved.
  \vspace*{\fill}
  \newpage
  \rm
}

\newcommand{\abstractpage}{
  \newpage
  \pagenumbering{roman}
  \setcounter{page}{3}
  \phantomsection
  \addcontentsline{toc}{chapter}{Abstract}
  \pagestyle{fancy}
  \lhead{Thesis advisor: Professor \@advisor} \rhead{\@author}
  \renewcommand{\headrulewidth}{0.0pt}
  % Abstract should begin like the other major section/chapter headings.
  % The dissertation title is intentionally omitted here.
  \chapter*{Abstract}
  \begin{doublespacing}
  \input{abstract}
  \end{doublespacing}
  \vspace*{\fill}
  \newpage \lhead{} \rhead{}
  \cfoot{\thepage}
}

\newcommand{\dedicationpage}{
  \newpage \thispagestyle{fancy} \vspace*{\fill}
  \normalfont\normalsize \noindent \input{dedication}
  \vspace*{\fill} \newpage \normalfont
}

\newcommand{\acknowledgments}{
  \chapter*{Acknowledgements}
  \phantomsection
  \addcontentsline{toc}{chapter}{Acknowledgements}
   \thispagestyle{empty}
  \begin{doublespacing}
  %\noindent
  \input{thanks}
  \end{doublespacing}
  \vspace*{\fill} \newpage
  % Keep Roman page numbering throughout the front matter.
  % Arabic numbering is started in dissertation.tex immediately before Chapter 1.
}
