# Term Project: Implementing a SnuPL/2 Compiler

In this term project, we implement a compiler for the SnuPL/2 language. 
The compiler accepts SnuPL/2 source code as an input and outputs to Intel assembly code.

SnuPL/2 is an imperative procedural language closely related to the [Oberon programming language](https://people.inf.ethz.ch/wirth/Oberon/index.html).
While SnuPL/2 does not support all standard features of a programming language such as floating point, user-defined data types, or pointer types, the language is complex enough to illustrate the basic concepts of a compiler.

## Language Specification
Refer to the [SnuPL/2 language specification](specification/SnuPL2.md).


## Phases

The project is split into the following phases:

| Phase | Topic                                                 | Handout                 | Deadline                       |
|:-----:|:------------------------------------------------------|:------------------------|:-------------------------------|
| 1     | [Lexical Analysis](assignments/1.Lexical.Analysis.md) | Wednesday, September 13 | Tuesday, September 26, 23:59   |
| 2     | [Syntax Analysis](assignments/2.Syntax.Analysis.md)   | Wednesday, September 27 | Tuesday, October 17, 23:59     |
| 3     | Semantic Analysis                                     | tba                     | tba                            |
| 4     | Intermediate Code Generation                          | tba                     | tba                            |
| 5     | Code Generation                                       | tba                     | tba                            |
| 6     | Project Report                                        |                         | tba                            |



## Logistics
The following instructions are valid for all phases of the project.

### Git
If you have never used Git before, you may want to have a look at one of the many tutorials out there.
W3Schools has a comprehensive, yet easy-to-understand [Git tutorial](https://www.w3schools.com/git/default.asp?remote=github).

We recommend to commit locally and push your work to our server frequently to avoid data loss. 

### Hand-out
This repository will be continuously updated phase by phase.

You need to fork this repository once into your namespace. To do so, click the 'Fork' button on top of this page and make sure the forked repository's visibility is set to 'Private.'

Whenever we publish new information about a next phase or upload bugfixes, you need to merge the new contents into your fork. To do so, follow steps outlined below

1. **Add the original (this) repository as the upstream**  
   You only need to do this once after cloning the repository.
   ```bash
   $ git remote add upstream https://teaching.csap.snu.ac.kr/compiler-construction/fall-2023/snupl2-compiler.git
   ```

2. **Fetch new contents from upstream**  
   ```bash
   $ git fetch upstream
   ```

3. **Merge or rebase the changes**  
   Read up on the [difference between merge and rebase](https://blog.git-init.com/differences-between-git-merge-and-rebase-and-why-you-should-care/) to know which one to pick.
   ```bash
   $ git merge upstream/main
   ```
   or  
   ```bash
   $ git rebase upstream/main
   ```

4. **Resolve conflicts (if any)**  
   Resolve any merge/rebase conflicts.

Steps 2-4 must be repeated for every hand-out.

For the phases that are conducted as a team, one of the team members should add the other team members as developers to ensure the entire team has access to the repository.


### Hand-in

Once you are happy with your implementation, commit and push all changes, then create an annotated tag as follows:
```bash
$ git tag -a phaseX -m "Submission Phase X". 
```

Do not forget to push your tag to the server, otherwise we won't see it
```bash
$ git push origin --tags
```

We consider the timestamp when the tag is pushed to the server as your submission time.
If you tag (i.e., submit) your code _after_ the submission deadline, please [email the TAs](mailto:compiler@csap.snu.ac.kr), so that they are aware your updated submission.

If you have already tagged your code, but later discover a problem and want to update your submission, you can delete and re-create the tag.
This can be done [from the command line](https://devconnected.com/how-to-delete-local-and-remote-tags-on-git/) or via the GitLab web interface (Repository -> Tags). 
Do not forget to re-tag your updated code and push the tag to the server.
