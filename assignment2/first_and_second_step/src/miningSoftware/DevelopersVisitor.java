package miningSoftware;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import br.com.metricminer2.domain.Commit;
import br.com.metricminer2.domain.Modification;
import br.com.metricminer2.persistence.PersistenceMechanism;
import br.com.metricminer2.scm.BlamedLine;
import br.com.metricminer2.scm.CommitVisitor;
import br.com.metricminer2.scm.SCMRepository;

public class DevelopersVisitor implements CommitVisitor {

	public HashMap<String, HashMap<String, Object>> lineResult = new HashMap();
	
	@Override
	public String name() {
		// TODO Auto-generated method stub
		return "developers";
	}

	@Override
	public void process(SCMRepository repo, Commit commit, PersistenceMechanism writer) {
		// TODO Auto-generated method stub
		Set<String> branch = commit.getBranches();
		Set<String> master = new HashSet<String>(Arrays.asList("master"));
		// list of modified files
		if(branch.equals(master))
		{
			String hashCode = commit.getHash();
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
				// determine file extension, we are only interested to inspect .java files in directory lucene/ (for LUCENE project)
				
				if(splitFolder[0].equals("lucene") && filename.substring(filename.length() - 5).equals(".java"))
				{
					// blame part
					List<BlamedLine> blame = repo.getScm().blame(list.getNewPath(), commit.getHash(), true);
					HashMap<String, Integer> listCommitterPerFile = new HashMap<String, Integer>();
					
					for(BlamedLine bl: blame)
					{
						// check if committer is already in the list, if not, create the list and add line contribution to 1
						if(!listCommitterPerFile.containsKey(bl.getCommitter()))
						{
							listCommitterPerFile.put(bl.getCommitter(),1);
						}
						// add line contribution to 1 
						else
						{
							listCommitterPerFile.put(bl.getCommitter(), listCommitterPerFile.get(bl.getCommitter()) + 1);
						}
					}
					// total line contributor per file
					int totalLineContributor = listCommitterPerFile.size();
					int minorLineContributor = 0;
					int majorLineContributor = 0;
					double maxOwnership = 0;
					String maxOwnershipName = "";
					double ownershipCurrentCommitter = 0;
					// compute total line contribution
					int totalLine = 0;
					for(HashMap.Entry<String, Integer> entry: listCommitterPerFile.entrySet())
					{
						totalLine += entry.getValue();
					}
					// compute minor major ownership things
					for(HashMap.Entry<String, Integer> e: listCommitterPerFile.entrySet())
					{
						double committerOwnership = (double) e.getValue()/totalLine;
						if(committerOwnership <= 0.05) minorLineContributor += 1;
						else majorLineContributor += 1;
						if(committerOwnership > maxOwnership) 
						{
							maxOwnership = committerOwnership;
							maxOwnershipName = e.getKey();
						}
					}
					// ownership of currentCommitter
					ownershipCurrentCommitter = (double) listCommitterPerFile.get(currentCommitter)/totalLine;
					// decide whether the max ownership committer is the current committer
					double authorMaxOwnership = 0;
					if( ownershipCurrentCommitter == maxOwnership) authorMaxOwnership = 1;
					
					HashMap<String, Object> x = new HashMap();
					double[] arrayLineContributor = new double[6];
					arrayLineContributor[0] = totalLineContributor;
					arrayLineContributor[1] = minorLineContributor;
					arrayLineContributor[2] = majorLineContributor;
					arrayLineContributor[3] = (double) Math.round(maxOwnership * 100)/100;
					arrayLineContributor[4] = (double) Math.round(ownershipCurrentCommitter * 100)/100;
					arrayLineContributor[5] = authorMaxOwnership;	
					
					x.put("commit", hashCode);
					x.put("fullpath", fullpath);
					x.put("filename", filename);
					x.put("folder", folder);
					x.put("committer", currentCommitter);
					x.put("timestamp", commitDatetime);
					x.put("list", arrayLineContributor);
					
					lineResult.put(hashCode+","+fullpath, x);
					
					writer.write(
						commit.getHash(),
						list.getFileName(),
						folder,
						commit.getCommitter().getName(),
						commitDatetime,
						totalLineContributor,
						minorLineContributor,
						majorLineContributor,
						(double) Math.round(maxOwnership * 100)/100,
						(double) Math.round(ownershipCurrentCommitter * 100)/100,
						authorMaxOwnership	
					);
				}
			}
		}
	}
}
