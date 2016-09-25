package miningSoftware;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.io.File;
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
		Calendar startDateLine = new GregorianCalendar(2013,11,01,00,00,00);
		Calendar endDateLine = new GregorianCalendar(2013,11,31,23,59,59);
		
		// date range for 2nd step
		Calendar startDateCommit = new GregorianCalendar(2013,11,01,00,00,00);
		Calendar endDateCommit = new GregorianCalendar(2013,11,31,23,59,59);
		
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
		
		DevelopersVisitor lineVisitor = new DevelopersVisitor();
		new RepositoryMining()
			.in(GitRepository.singleProject("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/lucene-solr"))
			.through(Commits.betweenDates(startDateLine, endDateLine))
			.process(lineVisitor, new CSVFile("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba.txt"))
			.mine();
		
		DevelopersVisitorCommit commitVisitor = new DevelopersVisitorCommit();
		new RepositoryMining()
			.in(GitRepository.singleProject("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/lucene-solr"))
			.through(Commits.betweenDates(startDateCommit, endDateCommit))
			.process(commitVisitor, new CSVFile("/Users/rezapermadi/Documents/LECTURE/Mining Software Repositories/Workspace/assignment2/coba_commit.txt"))
			.mine();
		
		for(HashMap.Entry<String, HashMap<String,Integer>> c: commitVisitor.fileContributor.entrySet())
		{
			String contributor = "(";
			for(HashMap.Entry<String, Integer> d: c.getValue().entrySet())
			{
				contributor = contributor + d.getKey() + ":" + d.getValue() + " ";
			}
			contributor = contributor + ")";
			System.out.println(c.getKey()+","+contributor);
		}
		
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
		
		String mapToJson = "";
		try {
			mapToJson = objectMapper.writeValueAsString(lineVisitor.lineResult);
		} catch (JsonProcessingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	
    	System.out.println("1. Convert Map to JSON :");
    	System.out.println(mapToJson);
    	
    	HashMap<String,HashMap<String, Object>> jsonToMap = new HashMap();
    	
    	TypeReference<HashMap<String, Object>> mapType = new TypeReference<HashMap<String,Object>>() {};
    	try {
			jsonToMap = objectMapper.readValue(mapToJson, mapType);
		} catch (JsonParseException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (JsonMappingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		//for(HashMap.Entry<String, HashMap<String, Object>> e: lineVisitor.lineResult.entrySet())
		for(HashMap.Entry<String, HashMap<String, Object>> e: jsonToMap.entrySet())		
    	{
			String output = "";
			output = output + e.getValue().get("commit") + ",";
			output = output + e.getValue().get("fullpath") + ",";
			output = output + e.getValue().get("filename") + ",";
			output = output + e.getValue().get("folder") + ",";
			output = output + e.getValue().get("committer") + ",";
			output = output + e.getValue().get("timestamp") + ",";
			double[] result = (double[]) e.getValue().get("list");
			for(int i=0; i<6; i++)
			{
				output = output + result[i] + ",";
			}
			System.out.println(output);
		}
	}
}