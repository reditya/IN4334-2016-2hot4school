package nl.lbercken;

import br.com.metricminer2.domain.Commit;
import br.com.metricminer2.domain.Modification;
import br.com.metricminer2.persistence.PersistenceMechanism;
import br.com.metricminer2.scm.BlamedLine;
import br.com.metricminer2.scm.CommitVisitor;
import br.com.metricminer2.scm.SCMRepository;
import com.atlassian.jira.rest.client.JiraRestClient;
import com.atlassian.jira.rest.client.NullProgressMonitor;
import com.atlassian.jira.rest.client.auth.AnonymousAuthenticationHandler;
import com.atlassian.jira.rest.client.domain.Issue;
import com.atlassian.jira.rest.client.internal.jersey.JerseyJiraRestClientFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BugInducingCommitsVisitor implements CommitVisitor {

    private final String[] bugKeywords = {"error", "bug", "fix", "issue", "mistake", "incorrect", "fault", "defect",
            "flaw", "typo"};

    @Override
    public void process(SCMRepository repo, Commit commit, PersistenceMechanism writer) {
        try {
            boolean bug = _isBug(commit.getMsg()), jiraBug = _isJiraBug(_extractIssueNumber(commit.getMsg()));
            if(bug || jiraBug) {
                for(Modification m : commit.getModifications()) {
                    if(m.fileNameEndsWith(".java") && m.fileNameMatches("^lucene\\/core\\/src\\/java.*")) {
                        // Search for changed lines
                        List<BlamedLine> lines = repo.getScm().blame(m.getFileName(), commit.getHash(), false);
                        List<Integer> changedLines = new ArrayList<>();
                        for(BlamedLine line : lines) {
                            if(line.getCommit().equals(commit.getHash())) {
                                // This line is changed by this commit
                                changedLines.add(line.getLineNumber());
                            }
                        }
                        // Look at each changed line
                        lines = repo.getScm().blame(m.getFileName(), commit.getHash(), true);
                        for(BlamedLine line : lines) {
                            for(Integer i : changedLines) {
                                if(line.getLineNumber() == i) {
                                    if(Miner.step3.containsKey(line.getCommit() + "," + m.getFileName())) {
                                        HashMap<String, Object> row = Miner.step3.get(line.getCommit() + "," + m.getFileName());
                                        row.put("bug_induced_qty", (int) row.get("bug_induced_qty") + 1);
                                        row.put("fix_commits_hash", row.get("fix_commits_hash") + "," + commit.getHash());
                                        row.put("fix_commits_timestamp", row.get("fix_commits_timestamp") + "," + commit.getDate().getTime().toString());
                                        if(bug) {
                                            row.put("dev_time_bug", (int) row.get("dev_time_bug") + 1);
                                        }
                                        if(jiraBug) {
                                            row.put("post_release_bug", (int) row.get("post_release_bug") + 1);
                                        }
                                        Miner.step3.put(line.getCommit() + "," + m.getFileName(), row);
                                    }
                                    else {
                                        HashMap<String, Object> row = new HashMap<>();
                                        row.put("dev_time_bug", 0);
                                        row.put("post_release_bug", 0);
                                        row.put("fix_commits_hash", commit.getHash());
                                        row.put("fix_commits_timestamp", commit.getDate().getTime().toString());
                                        row.put("bug_induced_qty", 1);
                                        if(bug) {
                                            row.put("dev_time_bug", 1);
                                        }
                                        if(jiraBug) {
                                            row.put("post_release_bug", 1);
                                        }
                                        Miner.step3.put(line.getCommit() + "," + m.getFileName(), row);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private boolean _isJiraBug(String issueNumber) throws URISyntaxException {
        if(issueNumber == "") {
            return false;
        }
        final JerseyJiraRestClientFactory factory = new JerseyJiraRestClientFactory();
        final URI jiraServerUri = new URI(Miner.JIRAURI);
        final JiraRestClient restClient = factory.create(jiraServerUri, new AnonymousAuthenticationHandler());
        final NullProgressMonitor pm = new NullProgressMonitor();
        final Issue issue = restClient.getIssueClient().getIssue(issueNumber, pm);

        if(issue.getIssueType().getName().equals("Bug")) {
            return true;
        }
        return false;
    }

    private boolean _isBug(String commitMsg) {
        for(String bugKeyword : bugKeywords) {
            if(commitMsg.toLowerCase().contains(bugKeyword.toLowerCase())) {
                return true;
            }
        }
        return false;
    }

    private String _extractIssueNumber(String msg) {
        // String pattern = "^(SOLR-[0-9]*|LUCENE-[0-9]*)";
        String pattern = "^(LUCENE-[0-9]*)";
        Pattern r = Pattern.compile(pattern);
        Matcher m = r.matcher(msg);
        if (m.find( )) {
            return m.group(0);
        }
        else {
            return "";
        }
    }

    @Override
    public String name() {
        return "Bug Inducing Commits";
    }

}