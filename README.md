# 🇺🇦 rada.git

Структурний парсинг та представлення документів rada.gov.ua у вигляді git-репозиторію. Більше деталей в [обґрунтуванні](https://github.com/vseloved/prj-nlp/pull/1).

## How to build and launch

Install https://haskellstack.org/ . Then:

```
cd rada-git
stack build
```

Take a look at [[rada-git/config.json]], if it looks good -- you can launch the project:

```
stack exec rada-git -- --help
```
