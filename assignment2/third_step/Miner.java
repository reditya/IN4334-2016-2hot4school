package nl.lbercken;

import br.com.metricminer2.MetricMiner2;
import br.com.metricminer2.RepositoryMining;
import br.com.metricminer2.Study;
import br.com.metricminer2.persistence.csv.CSVFile;
import br.com.metricminer2.scm.GitRepository;
import br.com.metricminer2.scm.commitrange.Commits;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class Miner implements Study {

    public static final String GITDIR = "C:\\Users\\Laurens\\Desktop\\lucene-solr";
    public static final String COMMIT2013 = "581effd152bb62fa1066aa4c8acab7a3e297c63d";
    public static final String COMMIT2016 = "8127db872fde946441d135f454114d84a9aa4eb7";
    public static final String JIRAURI = "https://issues.apache.org/jira/";

    public static HashMap<String, HashMap<String, Object>> step3;

    public static void main(String[] args) {
        // Initialize step3 hashmap
        step3 = new HashMap<>();

        new MetricMiner2().start(new Miner());
    }

    @Override
    public void execute() {
        new RepositoryMining()
                .in(GitRepository.singleProject(GITDIR))
                .through(Commits.range(COMMIT2013, COMMIT2016))
                .process(new BugInducingCommitsVisitor(), new CSVFile("C:\\Users\\Laurens\\Documents\\MSR\\bugs_inducing_commits.csv"))
                .mine();
        Iterator it = step3.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pair = (Map.Entry)it.next();
            printMap((Map) pair.getValue());
            it.remove(); // avoids a ConcurrentModificationException
        }
    }

    public static void printMap(Map mp) {
        Iterator it = mp.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pair = (Map.Entry)it.next();
            System.out.println(pair.getValue() + ",");
            it.remove(); // avoids a ConcurrentModificationException
        }
    }
}