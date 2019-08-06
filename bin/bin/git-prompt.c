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
    char *branch;
    if (strstr(first_line, "(no branch)") != NULL) {
        FILE *rev;
        rev = popen("git rev-parse --short HEAD", "r");
        char *rev_out;
        char *rev_parsed;
        n = 0;
        getline(&rev_out, &n, rev);
        sscanf(rev_out,"%m[^\n]", &rev_parsed);
        printf("%%F{10}:%s%%f", rev_parsed);
        free(rev_out);
        free(rev_parsed);
    }
    else if (strncmp(first_line, "## No commits", 13) == 0) {
        sscanf(first_line, "## No commits yet on %m[^\n]", &branch);
        printf("%%F{10}%s%%f", branch);
    }
    else {
        char *ahead_behind_str;
        int ahead_behind = 0;
        int behind = 0;
        int ret = sscanf(first_line,
                         "## %m[^.\n]...%*[^ ] %*c%ms %d, behind %d%*c",
                         &branch,
                         &ahead_behind_str,
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
