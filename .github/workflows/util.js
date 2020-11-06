const os = require('os');
const fs = require('fs');
const {sep, join, basename} = require('path');
const suffix = (os.type() === 'Windows_NT') ? '.bat' : '';

// Get raw download URL for path in this repo or pull-request repo
// (depending on context) automatically adds (.bat) extension to path
// if running on windows to make Github actions steps platform agnostic
function getRawUrl(context, path) {
    const {repository, pull_request} = context.payload;
    const branch = pull_request ? pull_request.head.ref : repository.default_branch;
    const repo_full_name = pull_request ? pull_request.head.repo.full_name : repository.full_name;
    return `https://raw.github.com/${repo_full_name}/${branch}/${path}${suffix}`;
}

// Download script and make it executable (required on Unix)
async function downloadScript(github, context, file, outdir) {
    const url = getRawUrl(context, file);
    const response = await github.request(url);
    const out = join(outdir, basename(file) + suffix);
    await fs.promises.writeFile(out, response.data);
    console.log(`Downloaded ${url} to ${out}`);
    if (os.type() !== 'Windows_NT') {
	fs.chmodSync(out, fs.constants.S_IXUSR | fs.constants.S_IRUSR);
    }
    return out;
}

// simple tilde expansion
function expandTilde(path) {
    return  path.replace('~', (os.type() === 'Windows_NT') ?  `${process.env.USERPROFILE}${sep}` : `${process.env.HOME}${sep}`);
}

module.exports = {
    getRawUrl,
    downloadScript,
    expandTilde
};
