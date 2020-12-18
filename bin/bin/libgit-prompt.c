#include <stdio.h>
#include <git2.h>

// Somehow, this is slower? than my hacky git-prompt.c But it's so much
// cleaner, and doesn't require calling another process?

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
    /* puts(path); */
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
