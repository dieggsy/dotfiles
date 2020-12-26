// This file contains two ways to produce a git shell prompt
//    - Using libgit2, which makes for much, MUCH cleaner code
//    - Parsing git status, which is ugly but somehow faster
#include <stdio.h>
#if defined __has_include
#  if __has_include (<git2.h>)
#include <git2.h>
void print_branch_info(git_repository *repo);
int status_cb(const char *path, unsigned int status_flags, void *payload);
void print_other_info(git_repository *repo);

typedef struct {
    int staged;
    int conflicts;
    int modified;
    int dirty;
} status_data;

int main () {
    git_libgit2_init();
    git_repository *repo = NULL;
    if (git_repository_open_ext(&repo, "./", 0, "/home:/tmp") == 0) {
        printf("(");
        print_branch_info(repo);
        printf("|");
        print_other_info(repo);
        printf(") ");
        /* git_status_list_new(&status, repo, opts); */
        /* printf("  %d", d.modified); */
    }
    git_libgit2_shutdown();
    return 0;
}

int status_cb(const char *path, unsigned int flags, void *payload) {
    (void)path;
    status_data *d = (status_data*)payload;
    if (flags & GIT_STATUS_WT_NEW) {
        d->dirty = 1;
    }
    if (flags & GIT_STATUS_WT_MODIFIED) {
        d->modified++;
    }
    if (flags & GIT_STATUS_CONFLICTED) {
        d->conflicts++;
    }
    if ((flags & GIT_STATUS_INDEX_NEW)
        || (flags & GIT_STATUS_INDEX_MODIFIED)
        || (flags & GIT_STATUS_INDEX_DELETED)
        || (flags & GIT_STATUS_INDEX_TYPECHANGE)
        || (flags & GIT_STATUS_INDEX_RENAMED)) {
        d->staged++;
    }
    return 0;
}

void print_other_info(git_repository *repo) {
    status_data d = {0};
    git_status_foreach(repo, status_cb, &d);
    /* if (d.staged>0) {} */
    if (!d.staged && !d.conflicts && !d.modified && !d.dirty) {
        printf("%%F{10}~%%f");
        return;
    };
    if (d.staged > 0) { printf("%%F{12}@%d%%f", d.staged); }
    if (d.modified > 0) { printf("%%F{11}#%d%%f", d.modified); }
    if (d.conflicts > 0) { printf("%%F{9}!%d%%f", d.conflicts); }
    if (d.dirty) { printf("*"); }
}

void print_branch_info(git_repository *repo) {
    git_reference *ref = NULL, *remote_ref = NULL;
    size_t ahead, behind;
    const char *branch_name = NULL;

    git_repository_head(&ref, repo);
    if (git_branch_name(&branch_name, ref) == 0) {
        printf("%%F{10}%s%%f", branch_name);
    } else {
        char hash[8];
        printf("%%F{10}:%s%%f",git_oid_tostr(hash,8,git_reference_target(ref)));
    };
    /* puts(branch_name); */
    if (git_branch_upstream(&remote_ref, ref) == 0) {
        git_graph_ahead_behind(&ahead, &behind, repo,
                               git_reference_target(ref),
                               git_reference_target(remote_ref));
        if (ahead > 0) {
            printf("%%F{13}+%zu%%f", ahead);
        }
        if (behind > 0) {
            printf("%%F{13}-%zu%%f", behind);
        }
    }
}
#else

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>

int regex_match(char *pattern, const char *string) {
    int status;
    regex_t re;

    if (regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB) != 0) {
        return 0;
    }
    status = regexec(&re, string, (size_t) 0, NULL, 0);
    regfree(&re);
    if (status != 0) {
        return 0;
    }
    return 1;
}

FILE* popenish (pid_t *pid, char* cmd[]) {
    enum { READ = 0, WRITE = 1};
    FILE* output;
    int pipefd[2];
    pipe(pipefd); //create a pipe

    *pid = fork(); //span a child process
    if (*pid == 0) {
        // Child. Let's redirect its standard output to our pipe and replace process with tail
        close(pipefd[READ]);
        dup2(pipefd[WRITE], STDOUT_FILENO);
        int dev_null = open("/dev/null", O_WRONLY);
        dup2(dev_null, STDERR_FILENO);
        execvp(cmd[0], cmd);
    }

    //Only parent gets here. Listen to what the tail says
    close(pipefd[WRITE]);
    output = fdopen(pipefd[READ], "r");

    return output;
}

void pcloseish(pid_t pid, FILE* file) {
    waitpid(pid,NULL,0);
    fclose(file);
}

void print_branch_info(FILE *status) {
    size_t n = 0;
    char *first_line = NULL;
    getline(&first_line, &n, status);
    if (strstr(first_line, "(no branch)") != NULL) {
        FILE *rev;
        pid_t pid;
        char *cmd[] = {"git", "rev-parse", "--short", "HEAD", NULL};
        rev = popenish(&pid, cmd);
        char *rev_out;
        n = 0;
        getline(&rev_out, &n, rev);
        char *rev_parsed = malloc(sizeof(char) * n);
        sscanf(rev_out,"%[^\n]", rev_parsed);
        printf("%%F{10}:%s%%f", rev_parsed);
        free(rev_out);
        free(rev_parsed);
        pcloseish(pid, rev);
        return;
    }
    char *branch = malloc(sizeof(char) * n);
    if (strncmp(first_line, "## No commits", 13) == 0) {
        sscanf(first_line, "## No commits yet on %[^\n]", branch);
        printf("%%F{10}%s%%f", branch);
    }
    else {
        char *ahead_behind_str = malloc(sizeof(char) * n);
        int ahead_behind = 0;
        int behind = 0;
        int ret = sscanf(first_line,
                         "## %[^.\n]...%*[^ ] %*c%s %d, behind %d%*c",
                         branch,
                         ahead_behind_str,
                         &ahead_behind,
                         &behind);
        switch (ret) {
            case 4 :
                printf("%%F{10}%s%%f%%F{13}+%d-%d%%f",
                       branch, ahead_behind, behind);
                free(ahead_behind_str);
                break;
            case 3 :
                if (strncmp(ahead_behind_str, "ahead", 5) == 0) {
                    printf("%%F{10}%s%%f%%F{13}+%d%%f", branch, ahead_behind);
                }
                else {
                    printf("%%F{10}%s%%f%%F{13}-%d%%f", branch, ahead_behind);
                }
                free(ahead_behind_str);
                break;
            default :
                printf("%%F{10}%s%%f", branch);
                break;
        }
    }
    free(branch);
    free(first_line);
}

void print_other_info(FILE *status) {
    int staged = 0;
    int conflicts = 0;
    int modified = 0;
    int dirty = 0;

    size_t n =0;
    char *first_line = NULL;
    while (getline(&first_line, &n, status) != -1) {
        if (regex_match("^(A[ DM]|C[ DM]|D[ M]|M[ DM]|R[ DM])", first_line)) {
            staged += 1;
        } else if (regex_match("^(A[AU]|D[DU]|U[ADU])", first_line)) {
            conflicts +=1;
        } else if (regex_match("^( [DM]|A[DM]|C[DM]|M[DM]|R[DM])",
                               first_line)) {
            modified += 1;
        } else if (strncmp("??",first_line,2) == 0) {
            dirty = 1;
        }
    };
    if (staged > 0) {
        printf("%%F{12}@%d%%f", staged);
    }
    if (conflicts > 0) {
        printf("%%F{9}!%d%%f", conflicts);
    }
    if (modified > 0) {
        printf("%%F{11}#%d%%f", modified);
    }
    if (dirty) {
        printf("*");
    }
    if (!staged && !conflicts && !modified && !dirty) {
        printf("%%F{10}~%%f");
    };
    free(first_line);
}

int main() {
    FILE *status;
    pid_t pid;
    char *cmd[] = {"git", "status", "--porcelain", "-b", NULL};
    status = popenish(&pid,cmd);
    int c;
    if ((c=getc(status)) != '#') {
        return 0;
    }
    ungetc(c,status);
    printf("(");
    print_branch_info(status);
    printf("|");
    print_other_info(status);
    printf(") ");
    pcloseish(pid, status);
    return 0;
}

#  endif
#endif
