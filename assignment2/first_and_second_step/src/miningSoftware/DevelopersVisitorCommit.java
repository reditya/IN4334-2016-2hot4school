package miningSoftware;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import br.com.metricminer2.domain.Commit;
import br.com.metricminer2.domain.Modification;
import br.com.metricminer2.persistence.PersistenceMechanism;
import br.com.metricminer2.scm.CommitVisitor;
import br.com.metricminer2.scm.SCMRepository;

public class DevelopersVisitorCommit implements CommitVisitor {

	@Override
	public String name() {
		// TODO Auto-generated method stub
		return null;
	}

	public HashMap<String, HashMap<String, Integer>> fileContributor = new HashMap();

	@Override
	public void process(SCMRepository repo, Commit commit, PersistenceMechanism writer) {
		// TODO Auto-generated method stub
		Set<String> branch = commit.getBranches();
		Set<String> master = new HashSet<String>(Arrays.asList("master"));
		// list of modified files
		if(branch.equals(master))
		{
			Calendar commitTimestamp = commit.getDate();
			SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			String commitDatetime = format.format(commitTimestamp.getTime());
			String currentCommitter = commit.getCommitter().getName();
			for(Modification list: commit.getModifications())
			{
				// derive the folder from full path name
				String filename = list.getFileName();
				String fullpath = list.getNewPath();
				String[] splitFolder = list.getNewPath().split("/");
				int length = splitFolder.length;
				String folder = "";
				for(int i = 0; i < length-1; i++)
				{
					folder = folder + splitFolder[i] + "/";
				}
				/*
				 * we will create data structure as follows
				 * filename, folder as a key with value a list
				 * containing committer and each contribution between 01-01-2011 until 31-12-2012
				 * it will be a predefined value for the next computation
				 */
				// check if filename exist or not
				if(splitFolder[0].equals("lucene") && splitFolder[1].equals("core") && 
						splitFolder[2].equals("src") && splitFolder[3].equals("java") && 
						filename.substring(filename.length() - 5).equals(".java"))
				{
					System.out.println(commitDatetime + " " + fullpath);
					HashMap<String, Integer> fileContributorValue = new HashMap<String, Integer>();
					if(fileContributor.containsKey(fullpath))
					{
						// check if the committer not exist
						fileContributorValue = fileContributor.get(fullpath);
						if(!fileContributorValue.containsKey(currentCommitter))
						{
							// add committer by one
							fileContributorValue.put(currentCommitter, 1);
							fileContributor.put(fullpath, fileContributorValue);
						}
						else
						{
							// add by one
							fileContributorValue = fileContributor.get(fullpath);
							fileContributorValue.put(currentCommitter, fileContributorValue.get(currentCommitter) + 1);
							fileContributor.put(fullpath, fileContributorValue);
						}
					}
					else
					{
						fileContributorValue.put(currentCommitter, 1);
						fileContributor.put(fullpath, fileContributorValue);
					}
				}
			}	
		}
	}
}
