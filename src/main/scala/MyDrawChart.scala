/*****************************
 * 特定のCSVの形式のファイルを、自動的にグラフ化するプログラム
 * args("グラフのタイトル","x軸のカラム名","y軸のカラム名,"対象ファイル"....)
 * 対象ファイル名がそのままグラフの名前になる
 ****************************/

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.data.general._
import org.jfree.data._
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.PiePlot
import javax.swing.JFrame
import javax.swing.JPanel
import scala.io._
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation


object MyDrawChart {

  
  //引数に指定されたカラムが、csvに存在するか
  def checkColumns(x: String,y:String, csvs: List[List[String]]):Boolean ={
    csvs match{
      case Nil => true
      case h::t => {
        val colums = h.head.split(",")
        if(colums.exists(_ == x) && colums.exists(_ == y)){
            checkColumns(x, y, t)
        }
        else false
      }
    }
  }
  
    //TODO: 適当すぎる!
    def getClumsNum(c:String,l: Array[String]):Int  ={
      	var i = 0
        for(e <- l){
          if(e == c) return i
          i = i + 1
        } 
      	return -1
    }
    
    //要素をDoubleに変換して返す
    def getElementD(num: Int,line: String): Double = {
      val line_splits = line.split(",")
      val a = line_splits(num).toDouble
      a
    }
    
    //要素を文字列に変換して返す
    def getElementS(num: Int,line: String): String = {
      val line_splits = line.split(",")
      val a = line_splits(num)
      a
    }
  
    //TODO: あんまり綺麗ではない、、、
  def setDataset(clums_num: (Int,Int),chart_info: (List[String],List[List[String]])): DefaultCategoryDataset = {
    var data = new DefaultCategoryDataset()
    val csvs = chart_info._2
    val chart_titles = chart_info._1
    var i = 0
    csvs.foreach(csv => { 
    			csv.tail.foreach(c => data.addValue(getElementD(clums_num._2, c), chart_titles(i), getElementS(clums_num._1,c)))
    			i = i + 1
    	}
    )
    data
  }
  
  //チャートを描く
  def drawLineChart(title: String,x: String,y: String,chart_info:(List[String],List[List[String]])) ={
    val clums = chart_info._2.head.head.split(",")
    var data = setDataset((getClumsNum(x,clums),getClumsNum(y,clums)),chart_info);
    
    var chart = ChartFactory.createLineChart(title, x, y, data, PlotOrientation.VERTICAL, true, false, false)
    
    val frame = new JFrame("Hello Pie World")
    	
	frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )

	frame.setSize(640,420)
	frame.add( new ChartPanel(chart) )
	frame.pack()
	frame.setVisible(true)
  }
  
  //ファイル名からグラフのタイトルを取得
  def getChartTitles(files: List[String]): List[String] = {
    files match {
      case Nil => Nil
      case h::t => h.split("/").last :: getChartTitles(t)
    }
  }
  
    
  //csvファイルを読み込む(複数可能)
  def readFromCSV(files:List[String]): List[List[String]] = {
    files match{
      case Nil => Nil
      case h::t => scala.io.Source.fromFile(h).getLines.toList :: readFromCSV(t)
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 3){
      print("args is too short")
      exit() //引数が3以下の時
    }
    val args_lit = args.toList
    val title = args_lit(0).toString() //表のタイトル
    val x = args_lit(1).toString() //x軸の名前(カラム名)
    val y = args_lit(2).toString() //y軸の名前(カラム名)
    val files = args_lit.slice(3, args_lit.size + 1) //ファイル名...
    val csvs = readFromCSV(files) //複数のファイル名から、複数csvを取得
    val chart_title = getChartTitles(files) //ファイル名からチャート名を取得
    if(checkColumns(x, y, csvs)){
    	drawLineChart(title,x, y, (chart_title,csvs))
    }
    else print("対象ファイルにカラムがありません")
  }
}