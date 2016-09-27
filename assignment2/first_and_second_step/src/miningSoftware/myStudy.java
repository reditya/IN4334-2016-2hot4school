package miningSoftware;

import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.*;
import br.com.metricminer2.*;
import br.com.metricminer2.persistence.csv.CSVFile;
import br.com.metricminer2.scm.CommitVisitor;
import br.com.metricminer2.scm.GitRepository;
import br.com.metricminer2.scm.commitrange.Commits;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;


public class myStudy implements Study {
	
	private static final String jsonFilePath = "/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/commit.json";

	public static void main(String[] args)
	{
		new MetricMiner2().start(new myStudy());
		
	}	
	
	@Override
	public void execute()
	{
		// date range for 1st step
		Calendar startDateLine = new GregorianCalendar(2013,00,01,00,00,00);
		Calendar endDateLine = new GregorianCalendar(2013,11,31,23,59,59);
		
		// date range for 2nd step
		Calendar startDateCommit = new GregorianCalendar(2012,8,01,00,00,00);
		Calendar endDateCommit = new GregorianCalendar(2012,11,31,23,59,59);
		
		try
		{
			File f = new File("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba.txt");
			File f2 = new File("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba_commit.txt");			
			if(f.exists()) f.delete();
			if(f2.exists()) f2.delete();			
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		
		// traverse through the repository to get value for line ownership
		DevelopersVisitor lineVisitor = new DevelopersVisitor();
		new RepositoryMining()
			.in(GitRepository.singleProject("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/lucene-solr"))
			.through(Commits.betweenDates(startDateLine, endDateLine))
			.process(lineVisitor, new CSVFile("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba.txt"))
			.mine();
		
		/*
		 * traverse from 2011 until before 2013 to get list of all committers contributed to a file up before 2013-01-01
		 * we will then do a foreach loop for result in step 1 with the prior value commit contributor got from this step
		 * however due to the limitation of the eclipse and memory in our computer, we can only run it from 1 Sep 2012 - 31 Dec 2012
		 * given more time and better computer performance, we believe that our approach can be extended until 1 Jan 2011
		*/
		
		DevelopersVisitorCommit commitVisitor = new DevelopersVisitorCommit();
		new RepositoryMining()
			.in(GitRepository.singleProject("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/lucene-solr"))
			.through(Commits.betweenDates(startDateCommit, endDateCommit))
			.process(commitVisitor, new CSVFile("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba_commit.txt"))
			.mine();
		
		
		String output = "";
		
		
		/*
		 * the idea is rather than travering each commit everytime, we want to save the result from the first traversal into step1.txt
		 * and then the result of second traversal into step2.txt that later we can call via Java.io
		 */
		for(HashMap.Entry<String, HashMap<String, Integer>> xx : commitVisitor.fileContributor.entrySet())
		{
			output = output + xx.getKey() + ",";
			for(HashMap.Entry<String, Integer> aa: xx.getValue().entrySet())
			{
				output = output + aa.getKey() + ":" + aa.getValue() + ",";
			}
			output = output + "\n";
		}
		
		File fileinput = new File("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/new_step2.txt");
		if(!fileinput.exists())
		{
			try {
				fileinput.createNewFile();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		try {
			FileWriter fw = new FileWriter(fileinput.getAbsoluteFile());
			BufferedWriter bw = new BufferedWriter(fw);
			bw.write(output);
			bw.close();
			System.out.println("Done");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		// iterate through lineVisitor to get ownership for commit contributor for each <commit, file> pair
		
		// test part
		String csvFile = "/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/step1.txt";
		String line = "";
		HashMap<String, HashMap<String, Object>> lineResult = new HashMap();
		try (BufferedReader br = new BufferedReader(new FileReader(csvFile)))
		{
			while ((line = br.readLine()) != null)
			{
				// separate by comma
				// System.out.println(line);
				String[] split = line.split(",");
				// create the hashmap
				HashMap<String, Object> x = new HashMap();
				// data structure for LineContributor
				double[] arrayLineContributor = new double[6];
				arrayLineContributor[0] = Double.parseDouble(split[5]);
				arrayLineContributor[1] = Double.parseDouble(split[6]);
				arrayLineContributor[2] = Double.parseDouble(split[7]);
				arrayLineContributor[3] = Double.parseDouble(split[8]);
				arrayLineContributor[4] = Double.parseDouble(split[9]);
				arrayLineContributor[5] = Double.parseDouble(split[10]);
				
				double[] listX = new double[]{0,0,0,0,0,0};
				x.put("commit", split[0]);
				x.put("fullpath", split[2]+split[1]);
				x.put("filename", split[1]);
				x.put("folder", split[2]);
				x.put("committer", split[3]);
				x.put("timestamp", split[4]);
				x.put("listLineContributor", arrayLineContributor);
				x.put("listCommitContributor", listX);
				
				lineResult.put(split[0]+","+split[2]+split[1], x);
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
		//end test part
		
		// read csv for file contributor
		String csvFileC = "/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/new_step2.txt";
		String lineC = "";
		HashMap<String, HashMap<String, Integer>> commitResult = new HashMap();
		try (BufferedReader br = new BufferedReader(new FileReader(csvFileC)))
		{
			while ((line = br.readLine()) != null)
			{
				// separate by comma
				//System.out.println(line);
				String[] split = line.split(",");
				int counter = 0;
				String key = "";
				HashMap<String, Integer> yy = new HashMap();
				for(String y: split)
				{
					if(counter == 0)
					{
						key = y;
						counter++;
					}
					else
					{
						String[] moresplit = y.split(":");
						//System.out.println(y);
						yy.put(moresplit[0], Integer.parseInt(moresplit[1]));
						counter++;
					}
				}
				commitResult.put(key, yy);
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
		//end test part
		
		commitVisitor.fileContributor = commitResult;
		
		for(HashMap.Entry<String, HashMap<String, Integer>> test2: commitResult.entrySet())
		{
			//System.out.println(test2.getValue());
		}
		
		
		//for(HashMap.Entry<String, HashMap<String, Object>> commitContributor: lineVisitor.lineResult.entrySet())
		for(HashMap.Entry<String, HashMap<String, Object>> test: lineResult.entrySet())
		{
			// System.out.println(test.getKey());
		}
		
		HashMap<String, HashMap<String, Object>> lineResultFinal = new HashMap();
		lineResultFinal = lineResult;
		
		//System.out.println(lineResult.entrySet());

		for(HashMap.Entry<String, HashMap<String, Object>> commitContributor: lineResult.entrySet())
		{
			int totalCommitContributor = 0;
			int totalCommit = 0;
			int minorCommitContributor = 0;
			int majorCommitContributor = 0;
			double maxCommitOwnership = 0;
			String maxCommitOwnershipName = "";
			double ownershipCommitCurrentCommitter = 0;
			double authorCommitMaxOwnership = 0;
			String currentCommitter = "";				
			double[] arrayCommitContributor = new double[6];

			String fullpathCommit = (String) commitContributor.getValue().get("fullpath");
			
			HashMap<String, Integer> listCommitter = new HashMap();
			//System.out.println(fullpathCommit);

			if(commitVisitor.fileContributor.containsKey(fullpathCommit))
			{
				listCommitter = commitVisitor.fileContributor.get(fullpathCommit);
				currentCommitter = (String) commitContributor.getValue().get("committer");
				// up to this point, we will get file contributor just before the commit
				// now we will compute the ownership
				totalCommitContributor = listCommitter.size();
				//System.out.println(fullpathCommit);
				//System.out.println(commitContributor.getKey());
				for(HashMap.Entry<String, Integer> d : listCommitter.entrySet())
				{
					totalCommit += d.getValue();
				}
				
				for(HashMap.Entry<String, Integer> d : listCommitter.entrySet())
				{
					double committerOwnership = (double) d.getValue()/totalCommit;
					if(committerOwnership <= 0.05) minorCommitContributor += 1;
					else majorCommitContributor += 1;
					if(committerOwnership > maxCommitOwnership) 
					{
						maxCommitOwnership = committerOwnership;
						maxCommitOwnershipName = d.getKey();
					}
				}
				// ownership of currentCommitter
				if(listCommitter.containsKey(currentCommitter)) ownershipCommitCurrentCommitter = (double) listCommitter.get(currentCommitter)/totalCommit;
				// decide whether the max ownership committer is the current committer
				if( ownershipCommitCurrentCommitter == maxCommitOwnership) authorCommitMaxOwnership = 1;
				
				// assign the value to each hashmap
				arrayCommitContributor[0] = totalCommitContributor;
				arrayCommitContributor[1] = minorCommitContributor;
				arrayCommitContributor[2] = majorCommitContributor;
				arrayCommitContributor[3] = (double) Math.round(maxCommitOwnership*100)/100;
				arrayCommitContributor[4] = (double) Math.round(ownershipCommitCurrentCommitter*100)/100;
				arrayCommitContributor[5] = authorCommitMaxOwnership;
			
				//lineVisitor.lineResult.get(commitContributor.getKey()).put("listCommitContributor", arrayCommitContributor);
				//lineResultFinal.get(commitContributor.getKey()).put("listCommitContributor", arrayCommitContributor);
				commitContributor.getValue().put("listCommitContributor", arrayCommitContributor);
				
			}
			else
			{
				arrayCommitContributor[0] = 0;
				arrayCommitContributor[1] = 0;
				arrayCommitContributor[2] = 0;
				arrayCommitContributor[3] = 0;
				arrayCommitContributor[4] = 0;
				arrayCommitContributor[5] = 0;
			
				//lineVisitor.lineResult.get(commitContributor.getKey()).put("listCommitContributor", arrayCommitContributor);
				//lineResultFinal.get(commitContributor.getKey()).put("listCommitContributor", arrayCommitContributor);			
				commitContributor.getValue().put("listCommitContributor", arrayCommitContributor);			
			}
			// time for output
			
			// at this point, we have got all the metric. it's time to increment filecontributor value by one for the committer
			HashMap<String, Integer> fileContributorValue = new HashMap<String, Integer>();
			if(commitVisitor.fileContributor.containsKey(fullpathCommit))
			{
				// check if the committer not exist
				fileContributorValue = commitVisitor.fileContributor.get(fullpathCommit);
				if(!fileContributorValue.containsKey(currentCommitter))
				{
					// add committer by one
					fileContributorValue.put(currentCommitter, 1);
					commitVisitor.fileContributor.put(fullpathCommit, fileContributorValue);
				}
				else
				{
					// add by one
					fileContributorValue = commitVisitor.fileContributor.get(fullpathCommit);
					fileContributorValue.put(currentCommitter, fileContributorValue.get(currentCommitter) + 1);
					commitVisitor.fileContributor.put(fullpathCommit, fileContributorValue);
				}
			}
			else
			{
				fileContributorValue.put(currentCommitter, 1);
				commitVisitor.fileContributor.put(fullpathCommit, fileContributorValue);
			}
			//System.out.println(fullpathCommit);
		}
		
		String finalResult = "";
		
		for(HashMap.Entry<String, HashMap<String, Object>> mm : lineResultFinal.entrySet())
		{
			String lines = "";
			String commits = "";
			double[] linesArray = (double[]) mm.getValue().get("listLineContributor");
			double[] commitsArray = (double[]) mm.getValue().get("listCommitContributor");
			
			String stateLines = "false";
			String stateCommits = "false";
			
			if(linesArray[5] == 1.0) stateLines = "true";
			
			if(commitsArray[5] == 1.0) stateCommits = "true";

			lines = Arrays.toString(linesArray);
			commits = Arrays.toString(commitsArray);
			
			System.out.println(
					mm.getValue().get("commit").toString() + "," +  
					mm.getValue().get("filename").toString() + "," +
					mm.getValue().get("folder").toString() + "," +
					mm.getValue().get("committer").toString() + "," +
					mm.getValue().get("timestamp").toString() + "," +
					linesArray[0] + "," +
					linesArray[1] + "," +
					linesArray[2] + "," +
					linesArray[3] + "," +
					linesArray[4] + "," +
					stateLines + "," +
					commitsArray[0] + "," +
					commitsArray[1] + "," +
					commitsArray[2] + "," +
					commitsArray[3] + "," +
					commitsArray[4] + "," +
					stateCommits
					);
		}		
	}
}