
package org.terra

import org.scalatest._
import org.scalatest.events._

import org.junit.{ BeforeClass, Test }
import org.junit.runner.{ RunWith, Result, Runner, Description }
import org.junit.runners.{ Suite => JUnit4Suite, ParentRunner }
import org.junit.runners.model.{ RunnerBuilder, Statement }
import org.junit.runner.notification.{ RunNotifier, Failure }

import com.carrotgarden.sjs.junit.{ 
  LinkerImpl, SuiteSetupImpl, TestInit, ScalaJS_Suite }
import TerraSuiteTest._

object Linker extends LinkerImpl
class SuiteSetup extends SuiteSetupImpl(Linker)
object SuiteSetup extends SuiteSetup

@RunWith(classOf[JUnit4Suite])
@JUnit4Suite.SuiteClasses(Array(
  classOf[TopSuite], classOf[ElectroSuite], classOf[EnergySuite], 
  classOf[InformationSuite], classOf[MassSuite], classOf[MotionSuite],
  classOf[MarketSuite], classOf[PhotoSuite], classOf[RadioSuite], 
  classOf[SpaceSuite], classOf[ThermalSuite], classOf[TimeSuite],
  classOf[CommonSuite], classOf[ClassicSuite], classOf[DoubleSuite]
))
class TerraSuiteTest

// TODO checks are not supported because scalacheck doesn't support Scala-JS
object TerraSuiteTest {

  @BeforeClass
  def setup: Unit = TestInit.setup(SuiteSetup)

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[QuantitySpec], classOf[BinarySystemSpec],
    classOf[MetricSystemSpec], classOf[RatioSpec], classOf[UnitOfMeasureSpec],
    classOf[DimensionlessSpec], classOf[QuantityRangeSpec], classOf[SVectorSpec]
  ))
  class TopSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[electro.AreaElectricChargeDensitySpec], 
    classOf[electro.CapacitanceSpec],
    classOf[electro.ConductivitySpec],
    classOf[electro.ElectricChargeDensitySpec],
    classOf[electro.ElectricChargeMassRatioSpec],
    classOf[electro.ElectricChargeSpec],
    classOf[electro.ElectricCurrentDensitySpec],
    classOf[electro.ElectricCurrentSpec],
    classOf[electro.ElectricFieldStrengthSpec],
    classOf[electro.ElectricPotentialSpec],
    classOf[electro.ElectricalConductanceSpec],
    classOf[electro.ElectricalResistanceSpec],
    //classOf[electro.ElectroChecks],
    classOf[electro.InductanceSpec],
    classOf[electro.LinearElectricChargeDensitySpec],
    classOf[electro.MagneticFieldStrengthSpec],
    classOf[electro.MagneticFluxDensitySpec],
    classOf[electro.MagneticFluxSpec],
    classOf[electro.PermeabilitySpec],
    classOf[electro.PermittivitySpec],
    classOf[electro.ResistivitySpec]))
  class ElectroSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[energy.EnergyAreaDensitySpec], 
    //classOf[energy.EnergyChecks],
    classOf[energy.EnergyDensitySpec], classOf[energy.EnergySpec], 
    classOf[energy.MolarEnergySpec], classOf[energy.PowerDensitySpec], 
    classOf[energy.PowerRampSpec], classOf[energy.PowerSpec],
    classOf[energy.SpecificEnergySpec]
    ))
  class EnergySuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[information.DataRateSpec], classOf[information.InformationSpec]))
  class InformationSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[mass.AreaDensitySpec], classOf[mass.CatalyticActivitySpec],
    classOf[mass.ChemicalAmountSpec], classOf[mass.ConcentrationSpec],
    classOf[mass.DensitySpec],
    //classOf[mass.MassChecks],
    classOf[mass.MolarMassSpec], classOf[mass.MassSpec], 
    classOf[mass.MolaritySpec], classOf[mass.MomentOfInertiaSpec]
    ))
  class MassSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[motion.AccelerationSpec], classOf[motion.AngularAccelerationSpec],
    classOf[motion.AngularVelocitySpec], classOf[motion.ForceSpec],
    classOf[motion.JerkSpec], classOf[motion.MassFlowSpec],
    classOf[motion.MomentumSpec], 
    //classOf[motion.MotionChecks],
    classOf[motion.PressureSpec], classOf[motion.PressureChangeSpec],
    classOf[motion.SurfaceTensionSpec], classOf[motion.TorqueSpec],
    classOf[motion.VelocitySpec], classOf[motion.ViscositySpec],
    classOf[motion.VolumeFlowSpec], classOf[motion.YankSpec]
  ))
  class MotionSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[market.CurrencyExchangeRateSpec], classOf[market.EmployeeSpec],
    classOf[market.LaborSpec], //classOf[market.MarketChecks],
    classOf[market.MoneyContextSpec], classOf[market.MoneySpec],
    classOf[market.PriceSpec]
  ))
  class MarketSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[photo.IlluminanceSpec], classOf[photo.LuminanceSpec],
    classOf[photo.LuminousEnergySpec], classOf[photo.LuminousExposureSpec],
    classOf[photo.LuminousFluxSpec], classOf[photo.LuminousIntensitySpec] //,
    //classOf[photo.PhotoChecks]
  ))
  class PhotoSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[radio.AbsorbedDoseSpec], classOf[radio.ActivitySpec],
    classOf[radio.AreaTimeSpec], classOf[radio.DoseSpec],
    classOf[radio.IrradianceSpec], classOf[radio.ParticleFluxSpec],
    classOf[radio.RadianceSpec], classOf[radio.RadiantIntensitySpec],
    //classOf[radio.RadioChecks],
    classOf[radio.SpectralIntensitySpec], classOf[radio.SpectralIrradianceSpec],
    classOf[radio.SpectralPowerSpec]
  ))
  class RadioSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[space.AngleSpec], classOf[space.AreaSpec], 
    classOf[space.LengthSpec], classOf[space.MolarVolumeSpec],
    classOf[space.SolidAngleSpec],
    //classOf[space.SpaceChecks],
    classOf[space.SpecificVolumeSpec], classOf[space.VolumeSpec]
  ))
  class SpaceSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[thermal.TemperatureSpec], classOf[thermal.ThermalCapacitySpec] //,
    //classOf[thermal.ThermalChecks]
  ))
  class ThermalSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[time.TimeSpec], classOf[time.TimeSquaredSpec], 
    classOf[time.FrequencySpec]
  ))
  class TimeSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[classic.DataRateSpec], classOf[classic.DimensionlessSpec],
    classOf[classic.FrequencySpec], classOf[classic.InformationSpec],
    classOf[classic.TimeSpec], classOf[classic.TimeSquaredSpec]
  ))
  class ClassicSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(classOf[common.TimeLongSpec]))
  class CommonSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(classOf[double.InformationDoubleSpec]))
  class DoubleSuite
}

class ScalaJS_ScalatestSuite(klaz: Class[_], builder: RunnerBuilder) 
    extends JUnit4Suite(klaz, builder) {

  import collection.JavaConversions._

  protected override def getChildren(): java.util.List[Runner] =
    klaz.getAnnotationsByType(classOf[JUnit4Suite.SuiteClasses]).flatMap {
      _.value.map { clz => {
        val suite = clz.newInstance().asInstanceOf[Suite]
        new ScalatestRunner(clz.asInstanceOf[Class[Suite]], suite) } } }.toSeq

  class ScalatestRunner(clazz: Class[Suite], scalatestSuite: Suite)
      extends ScalaJS_Suite(clazz, Seq(this)) {

    lazy val desc =
      Description.createTestDescription(clazz, scalatestSuite.suiteName)

    override def run(notifier: RunNotifier): Unit = {
      val rep = new Reporter {
        def apply(event: Event): Unit = event match {
          case ts: TestStarting => notifier.fireTestStarted(
            Description.createTestDescription(clazz, ts.testText))
          case ts: TestSucceeded => notifier.fireTestFinished(
            Description.createTestDescription(clazz, ts.testText))
          case ti: TestIgnored => notifier.fireTestIgnored(
            Description.createTestDescription(clazz, ti.testText))
          case tf: TestFailed =>
            notifier.fireTestFailure(new Failure(
              Description.createTestDescription(clazz, tf.testText),
              tf.throwable.getOrElse(new Exception(tf.message))))
          case _ => // do nothing
        }
      }
      scalatestSuite.run(None, Args(reporter = rep))
    }

    override def getDescription(): Description = desc
    override def testCount(): Int = scalatestSuite.testNames.size

    override def toString(): String = s"ScalatestRunner[${clazz.getName()}]"
  }
}
