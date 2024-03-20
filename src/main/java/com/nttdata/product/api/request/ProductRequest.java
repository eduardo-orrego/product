package com.nttdata.product.api.request;


import com.nttdata.product.enums.CurrencyTypeEnum;
import com.nttdata.product.enums.ProductTypeEnum;
import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Class: ProductRequest. <br/>
 * <b>Bootcamp NTTDATA</b><br/>
 *
 * @author NTTDATA
 * @version 1.0
 *   <u>Developed by</u>:
 *   <ul>
 *   <li>Developer Carlos</li>
 *   </ul>
 * @since 1.0
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductRequest {

  @NotNull
  private ProductTypeEnum type;

  @NotNull
  private CurrencyTypeEnum currency;

  @NotNull
  private BigDecimal interestRate;

  private BigDecimal minimumOpeningAmount;
  private BigDecimal minimumAmountPersonalVip;
  private BigDecimal maintenanceCommission;
  private Integer monthlyLimitMovement;
  private Integer limitFreeMovements;
  private BigDecimal commissionMovement;
  private Integer specificDayMonthMovement;

}
