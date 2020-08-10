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
\RequirePackage[width=5.75in, letterpaper]{geometry}
\usepackage{ragged2e}
\RaggedRight
\RequirePackage{graphicx}
\usepackage{fixltx2e}
\parindent 12pt
\RequirePackage{lettrine}
\RequirePackage{setspace}
\RequirePackage{verbatim}

% No ident for new paragraph
\usepackage[parfill]{parskip}

% Fonts.
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{lmodern}
%\usepackage[english]{babel}
\usepackage{blindtext}
\usepackage{helvet}
\RequirePackage{color}
%\RequirePackage{hyperref}
\RequirePackage{url}
\RequirePackage{amssymb}
\widowpenalty=300
\clubpenalty=300
\renewcommand{\thefootnote}{\fnsymbol{footnote}}

% Headings and headers.
\RequirePackage{fancyhdr}
\RequirePackage[tiny, md, sc]{titlesec}
\setlength{\headheight}{15pt}
\pagestyle{plain}
\RequirePackage{titling}

%citations
\usepackage[breaklinks=true]{hyperref}
\usepackage{breakcites}
\usepackage{url}
\usepackage{cite}
%\usepackage{natbib}

%math
\usepackage[fleqn]{amsmath}
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
\usepackage{geometry}

% Add source code
\usepackage{listings}

% Add pdf file
\usepackage{pdfpages} 

% Front matter.
\setcounter{tocdepth}{1}
\usepackage[titles]{tocloft}
\usepackage[titletoc]{appendix}
\renewcommand{\cftsecleader}{\cftdotfill{\cftdotsep}}
\renewcommand{\cftchapfont}{\normalsize \scshape}
\renewcommand\listfigurename{Listing of figures}
\renewcommand\listtablename{Listing of tables}

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

\newcommand{\copyrightpage}{
  \newpage
  \thispagestyle{empty}
  \vspace*{\fill}
  \scshape \noindent \small \copyright \small 2014\hspace{3pt}-- \theauthor \\
  \noindent all rights reserved.
  \vspace*{\fill}
  \newpage
  \rm
}

\newcommand{\abstractpage}{
  \newpage
  \pagenumbering{roman}
  \setcounter{page}{3}
  \pagestyle{fancy}
  \lhead{Thesis advisor: Professor \@advisor} \rhead{\@author}
  \renewcommand{\headrulewidth}{0.0pt}
  \vspace*{35pt}
  \begin{center}
  \Large {\@title} \normalsize \\
  \vspace*{20pt}
  \scshape Abstract \\ \rm
  \end{center}
  \input{abstract}
  \vspace*{\fill}
  \newpage \lhead{} \rhead{}
  \cfoot{\thepage}
}

\newcommand{\dedicationpage}{
  \newpage \thispagestyle{fancy} \vspace*{\fill}
  \scshape \noindent \input{dedication}
  \vspace*{\fill} \newpage \rm
}

\newcommand{\acknowledgments}{
  \chapter*{Acknowledgments}
  \noindent
  \input{thanks}
  \vspace*{\fill} \newpage
  \setcounter{page}{1}
  \pagenumbering{arabic}
}
